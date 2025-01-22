from django.shortcuts import render


def index_view(request):
    context = {}
    return render(request, 'index.html', context)


def one_view(request):
    context = {}
    return render(request, 'one.html', context)


def two_view(request):
    context = {}
    return render(request, 'two.html', context)


def offline_view(request):
    context = {}
    return render(request, 'offline2.html', context)
