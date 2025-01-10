importScripts('https://storage.googleapis.com/workbox-cdn/releases/6.5.4/workbox-sw.js');

if (workbox) {
    console.log(`Workbox is loaded`);

    // Precache static assets
    workbox.precaching.precacheAndRoute([
        {url: '/offline', revision: '1'},
        {url: '/static/css/django-pwa-app.css', revision: '1'},
        {url: '/static/images/icons/icon-72x72.png', revision: '1'},
        {url: '/static/images/icons/icon-96x96.png', revision: '1'},
        {url: '/static/images/icons/icon-128x128.png', revision: '1'},
        {url: '/static/images/icons/icon-192x192.png', revision: '1'},
        {url: '/static/images/icons/icon-512x512.png', revision: '1'},
        {url: '/static/images/screenshots/desktop.png', revision: '1'},
        {url: '/static/images/screenshots/mobile.png', revision: '1'},
        // Add more assets here...
    ]);

    // Cache CSS and JS files
    workbox.routing.registerRoute(
        ({request}) => request.destination === 'style' || request.destination === 'script',
        new workbox.strategies.StaleWhileRevalidate({
            cacheName: 'static-resources',
        })
    );

    // Cache images
    workbox.routing.registerRoute(
        ({request}) => request.destination === 'image',
        new workbox.strategies.CacheFirst({
            cacheName: 'image-cache',
            plugins: [
                new workbox.expiration.ExpirationPlugin({
                    maxEntries: 50,
                    maxAgeSeconds: 30 * 24 * 60 * 60, // 30 Days
                }),
            ],
        })
    );

    // Offline fallback
    workbox.routing.setCatchHandler(({event}) => {
        if (event.request.destination === 'document') {
            return caches.match('/offline');
        }
        return Response.error();
    });
} else {
    console.error(`Workbox failed to load`);
}
