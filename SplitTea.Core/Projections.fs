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

module Projections =
    let private round2 (x: decimal) =
        System.Math.Round(x, 2, System.MidpointRounding.AwayFromZero)

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
            shares
        | Percentage shares ->
            shares |> Map.map (fun _ pct -> round2 (amount * pct / 100m))
        | Shares shares ->
            let total = shares |> Map.toSeq |> Seq.sumBy snd
            shares |> Map.map (fun _ s -> round2 (amount * decimal s / decimal total))

    let private addTo (memberId: MemberId) (delta: decimal) (m: Map<MemberId, decimal>) =
        let current = Map.tryFind memberId m |> Option.defaultValue 0m
        Map.add memberId (current + delta) m

    let computeNetPositions (state: GroupState) : NetPosition list =
        let init = state.Members |> Map.map (fun _ _ -> 0m)

        let afterExpenses =
            state.Expenses
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.filter (fun e -> not e.IsDeleted)
            |> Seq.fold (fun pos expense ->
                let shares = expandSplit expense.Amount expense.Split expense.PaidBy
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
                pos |> addTo s.From s.Amount |> addTo s.To -s.Amount
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
