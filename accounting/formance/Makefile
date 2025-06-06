export PYTHONPATH := .

.PHONY: install test migrate dev create_admin

install:
	pip install -r requirements.txt

test:
	pytest

migrate:
	python manage.py makemigrations
	python manage.py migrate

dev: migrate
	python manage.py runserver

create_admin:
	python manage.py createsuperuser
