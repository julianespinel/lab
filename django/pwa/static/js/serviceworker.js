// Give your cache a unique name
const CACHE_NAME = 'my-pwa-cache-v1';

// List of files to cache
const urlsToCache = [
  '/',
  '/offline',
  '/one',
  '/static/css/django-pwa-app.css',
  '/static/images/icons/icon-192x192.png'
];

// Install event
self.addEventListener('install', event => {
  event.waitUntil(
    caches.open(CACHE_NAME)
      .then(cache => {
        console.log('Opened cache');
        return cache.addAll(urlsToCache);
      })
  );
});

// Fetch event
self.addEventListener('fetch', event => {
  event.respondWith(
    caches.match(event.request)
      .then(response => {
        // Return cached response if found
        if (response) {
          return response;
        }

        // Clone the request because it can only be used once
        const fetchRequest = event.request.clone();

        // Try to fetch from network
        return fetch(fetchRequest)
          .then(response => {
            // Check if response is valid and not /two route
            if (!response || response.status !== 200 || response.type !== 'basic' || event.request.url.endsWith('/two')) {
              return response;
            }

            // Clone the response because it can only be used once
            const responseToCache = response.clone();

            // Add response to cache
            caches.open(CACHE_NAME)
              .then(cache => {
                cache.put(event.request, responseToCache);
              });

            return response;
          })
          .catch(() => {
            // If fetch fails (offline), show offline page
            if (event.request.mode === 'navigate') {
              return caches.match('/offline');
            }
          });
      })
  );
});
