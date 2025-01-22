install:
	pip install -r requirements.txt

test:
	python manage.py test

collectstatic:
	python manage.py collectstatic --no-input --clear

run:
	python manage.py makemigrations
	python manage.py migrate
	python manage.py runserver
