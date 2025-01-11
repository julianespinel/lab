from django.urls import path

from .views import index_view, offline_view

urlpatterns = [
    path('', index_view, name='index'),
    path('offline', offline_view, name='offline'),
]
