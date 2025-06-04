from django import forms
from django.contrib.auth.models import User
from crispy_forms.helper import FormHelper
from crispy_forms.layout import Layout, Submit, Row, Column
from decimal import Decimal
from .models import Transaction


class SendTransactionForm(forms.ModelForm):
    class Meta:
        model = Transaction
        fields = ['destination_user', 'amount']

    destination_user = forms.ModelChoiceField(
        queryset=User.objects.all(),
        empty_label="Select recipient",
        widget=forms.Select(attrs={'class': 'form-control'})
    )

    amount = forms.DecimalField(
        max_digits=15,
        decimal_places=2,
        min_value=Decimal('0.01'),
        widget=forms.NumberInput(attrs={
            'class': 'form-control',
            'step': '0.01',
            'placeholder': '0.00'
        })
    )

    def __init__(self, *args, current_user=None, **kwargs):
        super().__init__(*args, **kwargs)

        # Store current user for validation
        self.current_user = current_user

        # Exclude current user from destination choices
        if current_user:
            self.fields['destination_user'].queryset = User.objects.exclude(
                id=current_user.id
            )

        self.helper = FormHelper()
        self.helper.form_method = 'post'
        self.helper.layout = Layout(
            Row(
                Column('destination_user', css_class='form-group col-md-8'),
                Column('amount', css_class='form-group col-md-4'),
                css_class='form-row'
            ),
            Submit('submit', 'Send Transaction',
                   css_class='btn btn-primary w-100 mt-3')
        )

    def clean_destination_user(self):
        """Ensure user cannot send transaction to themselves"""
        destination_user = self.cleaned_data.get('destination_user')

        if self.current_user and destination_user == self.current_user:
            raise forms.ValidationError(
                "You cannot send a transaction to yourself."
            )

        return destination_user
