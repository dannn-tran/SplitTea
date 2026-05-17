namespace SplitTea.Core

type NetPosition = {
    MemberId: MemberId
    Amount: Amount  // positive = owed to this member; negative = owes others
}

type SuggestedSettlement = {
    From: MemberId
    To: MemberId
    Amount: Amount
}

type CategorySpend = {
    Category : string
    Total    : Amount
}

type MemberSpend = {
    MemberId : MemberId
    Total    : Amount
}

type WeekSpend = {
    Year  : int
    Week  : int   // ISO week number
    Label : string
    Total : Amount
}

type MonthSpend = {
    Year  : int
    Month : int
    Label : string
    Total : Amount
}

module Projections =
    let private round2 (x: decimal) =
        System.Math.Round(x, 2, System.MidpointRounding.AwayFromZero)

    let private workingAmount (e: ExpenseState) (groupCurrency: CurrencyCode) : Amount option =
        if e.PaidCurrency = groupCurrency then Some e.PaidAmount
        else e.ExchangeRate |> Option.map (fun r -> e.PaidAmount * r)

    let private settlementWorkingAmount (s: SettlementRecordedPayload) (groupCurrency: CurrencyCode) : Amount option =
        if s.Currency = groupCurrency then Some s.Amount
        else s.ExchangeRate |> Option.map (fun r -> s.Amount * r)

    // Returns per-member share amounts summing exactly to `amount`.
    // Payer absorbs any rounding remainder so non-payer obligations are exact.
    let private expandSplit (amount: Amount) (split: Split) (paidBy: MemberId) : Map<MemberId, Amount> =
        match split with
        | Equal members ->
            let n = List.length members
            // Ceiling rounding: non-payers round up, payer absorbs any negative remainder
            let share = System.Math.Ceiling(amount * 100m / decimal n) / 100m
            let base' = members |> List.map (fun m -> m, share) |> Map.ofList
            let remainder = amount - share * decimal n
            let payerShare = Map.tryFind paidBy base' |> Option.defaultValue 0m
            Map.add paidBy (payerShare + remainder) base'
        | Exact shares ->
            shares |> Map.ofList
        | Percentage shares ->
            shares |> List.map (fun (m, pct) -> m, round2 (amount * pct / 100m)) |> Map.ofList
        | Shares shares ->
            let total = shares |> List.sumBy snd
            shares |> List.map (fun (m, s) -> m, round2 (amount * decimal s / decimal total)) |> Map.ofList

    let private addTo (memberId: MemberId) (delta: decimal) (m: Map<MemberId, decimal>) =
        let current = Map.tryFind memberId m |> Option.defaultValue 0m
        Map.add memberId (current + delta) m

    let computeNetPositions (state: SpaceState) : NetPosition list =
        let init = state.Members |> Map.map (fun _ _ -> 0m)

        let afterExpenses =
            state.Expenses
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun e -> not e.IsDeleted)
            |> Seq.fold (fun pos expense ->
                match workingAmount expense state.Currency with
                | None -> pos  // unresolved foreign-currency expense; skip
                | Some amt ->
                    let shares = expandSplit amt expense.Split expense.PaidBy
                    shares
                    |> Map.toSeq
                    |> Seq.filter (fun (m, _) -> m <> expense.PaidBy)
                    |> Seq.fold (fun p (memberId, share) ->
                        p |> addTo expense.PaidBy share |> addTo memberId -share
                    ) pos
            ) init

        let afterSettlements =
            state.Settlements
            |> List.fold (fun pos s ->
                match settlementWorkingAmount s state.Currency with
                | None     -> pos  // unresolved foreign-currency settlement; skip
                | Some amt -> pos |> addTo s.From amt |> addTo s.To -amt
            ) afterExpenses

        afterSettlements
        |> Map.toList
        |> List.map (fun (id, amt) -> { MemberId = id; Amount = amt })

    // Greedy creditor/debtor matching — minimises number of transfers.
    let computeMinimumSettlements (positions: NetPosition list) : SuggestedSettlement list =
        let creditors = positions |> List.filter (fun p -> p.Amount > 0m) |> List.sortByDescending (fun p -> p.Amount)
        let debtors   = positions |> List.filter (fun p -> p.Amount < 0m) |> List.sortBy (fun p -> p.Amount)

        let rec go creds debs acc =
            match creds, debs with
            | [], _ | _, [] -> List.rev acc
            | (cred: NetPosition) :: restCreds, (deb: NetPosition) :: restDebs ->
                let settleAmt = min cred.Amount (-deb.Amount)
                let settlement : SuggestedSettlement = { From = deb.MemberId; To = cred.MemberId; Amount = settleAmt }
                let newCreds =
                    if cred.Amount > settleAmt
                    then { MemberId = cred.MemberId; Amount = cred.Amount - settleAmt } :: restCreds
                    else restCreds
                let newDebs =
                    if -deb.Amount > settleAmt
                    then { MemberId = deb.MemberId; Amount = deb.Amount + settleAmt } :: restDebs
                    else restDebs
                go newCreds newDebs (settlement :: acc)

        go creditors debtors []

    let private activeExpenses (state: SpaceState) =
        state.Expenses
        |> Map.toList
        |> List.map snd
        |> List.filter (fun e -> not e.IsDeleted)

    let computeSpendingByCategory (state: SpaceState) : CategorySpend list =
        activeExpenses state
        |> List.choose (fun e -> workingAmount e state.Currency |> Option.map (fun amt -> e.Category, amt))
        |> List.groupBy (fun (cat, _) -> cat |> Option.defaultValue "Uncategorized")
        |> List.map (fun (cat, xs) -> { Category = cat; Total = xs |> List.sumBy snd })
        |> List.sortByDescending (fun c -> c.Total)

    let computeSpendingByPayer (state: SpaceState) : MemberSpend list =
        activeExpenses state
        |> List.choose (fun e -> workingAmount e state.Currency |> Option.map (fun amt -> e.PaidBy, amt))
        |> List.groupBy fst
        |> List.map (fun (mid, xs) -> { MemberId = mid; Total = xs |> List.sumBy snd })
        |> List.sortByDescending (fun m -> m.Total)

    // ─── Date formatting helpers (Fable-compatible, no DateOnly.ToString) ────────

    let private monthAbbr = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
    let private monthFull = [| "January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December" |]

    // ─── ISO week helpers (pure arithmetic, no GregorianCalendar) ────────────

    let private isoWeek (d: System.DateOnly) : int * int =
        // Mon=1 … Sun=7; ISO 8601 formula
        let dow = match d.DayOfWeek with System.DayOfWeek.Sunday -> 7 | x -> int x
        let week = (d.DayOfYear - dow + 10) / 7
        if week < 1 then
            let prevDec31 = System.DateOnly(d.Year - 1, 12, 31)
            let prevDec31dow = match prevDec31.DayOfWeek with System.DayOfWeek.Sunday -> 7 | x -> int x
            d.Year - 1, if prevDec31dow >= 4 then 53 else 52
        elif week > 52 then
            let dec31 = System.DateOnly(d.Year, 12, 31)
            let dec31dow = match dec31.DayOfWeek with System.DayOfWeek.Sunday -> 7 | x -> int x
            if dec31dow >= 4 then d.Year, week else d.Year + 1, 1
        else
            d.Year, week

    let private weekLabel (year: int) (week: int) (expenses: ExpenseState list) : string =
        match expenses with
        | [] -> sprintf "Week %d · %d" week year
        | e :: _ ->
            let d = e.Date
            let dow = match d.DayOfWeek with System.DayOfWeek.Sunday -> 7 | x -> int x
            let monday = d.AddDays(1 - dow)
            let sunday = monday.AddDays(6)
            let fmtMD (x: System.DateOnly) = sprintf "%s %d" monthAbbr.[x.Month - 1] x.Day
            sprintf "Week %d · %s %d – %s %d" week (fmtMD monday) monday.Year (fmtMD sunday) sunday.Year

    let private monthLabel (year: int) (month: int) : string =
        sprintf "%s %d" monthFull.[month - 1] year

    // ─── Time-series spend projections ───────────────────────────────────────

    let computeSpendingByWeek (state: SpaceState) : WeekSpend list =
        activeExpenses state
        |> List.choose (fun e -> workingAmount e state.Currency |> Option.map (fun amt -> e, amt))
        |> List.groupBy (fun (e, _) -> isoWeek e.Date)
        |> List.map (fun ((year, week), xs) ->
            let expenses = xs |> List.map fst
            { Year  = year
              Week  = week
              Label = weekLabel year week expenses
              Total = xs |> List.sumBy snd })
        |> List.sortBy (fun w -> w.Year, w.Week)

    let computeSpendingByMonth (state: SpaceState) : MonthSpend list =
        activeExpenses state
        |> List.choose (fun e -> workingAmount e state.Currency |> Option.map (fun amt -> e, amt))
        |> List.groupBy (fun (e, _) -> e.Date.Year, e.Date.Month)
        |> List.map (fun ((year, month), xs) ->
            { Year  = year
              Month = month
              Label = monthLabel year month
              Total = xs |> List.sumBy snd })
        |> List.sortBy (fun m -> m.Year, m.Month)
