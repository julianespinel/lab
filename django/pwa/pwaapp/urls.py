from django.urls import path

from .views import index_view, offline_view, one_view, two_view

urlpatterns = [
    path('', index_view, name='index'),
    path('one', one_view, name='one'),
    path('two', two_view, name='two'),
    path('offline', offline_view, name='offline'),
]
