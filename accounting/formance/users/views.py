from django.contrib import messages
from django.contrib.auth.views import LoginView
from django.contrib.auth import logout
from django.shortcuts import redirect
from django.urls import reverse_lazy
from django.views.generic import CreateView
from .forms import UserRegistrationForm, UserLoginForm


def logout_view(request):
    """Custom logout view that handles GET requests"""
    logout(request)
    return redirect('users:login')


class CustomLoginView(LoginView):
    form_class = UserLoginForm
    template_name = 'users/login.html'


class RegisterView(CreateView):
    form_class = UserRegistrationForm
    template_name = 'users/register.html'
    success_url = reverse_lazy('users:login')

    def form_valid(self, form):
        form.save()
        messages.success(
            self.request,
            'Account created successfully! You can now log in.'
        )
        return super().form_valid(form)
