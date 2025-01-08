from django.shortcuts import render
from django.utils import timezone


# Create your views here.
def index_view(request):
    context = {
        'greeting': "Welcome to our Localization Project!",
        'large_number': 12345.67,
        'current_date': timezone.now()
    }
    return render(request, 'index.html', context)
