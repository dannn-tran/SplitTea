const CACHE = 'splittea-v1'

self.addEventListener('install', e => {
  // Pre-cache the app shell so the first offline load works.
  e.waitUntil(caches.open(CACHE).then(c => c.add('/')))
  self.skipWaiting()
})

self.addEventListener('activate', e => {
  // Delete old cache versions.
  e.waitUntil(
    caches.keys().then(keys =>
      Promise.all(keys.filter(k => k !== CACHE).map(k => caches.delete(k)))
    )
  )
  self.clients.claim()
})

self.addEventListener('fetch', e => {
  const { request } = e
  if (request.method !== 'GET') return
  const url = new URL(request.url)

  // Only intercept same-origin requests; let Supabase calls pass through.
  if (url.origin !== self.location.origin) return

  if (url.pathname.startsWith('/assets/')) {
    // Hashed Vite assets: cache-first. The hash is the freshness guarantee.
    e.respondWith(
      caches.match(request).then(hit => {
        if (hit) return hit
        return fetch(request).then(res => {
          const clone = res.clone()
          caches.open(CACHE).then(c => c.put(request, clone))
          return res
        })
      })
    )
  } else {
    // App shell (index.html, icons, manifest): network-first so updates land immediately.
    e.respondWith(
      fetch(request)
        .then(res => {
          const clone = res.clone()
          caches.open(CACHE).then(c => c.put(request, clone))
          return res
        })
        .catch(() => caches.match(request))
    )
  }
})
