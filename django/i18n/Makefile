install:
	pip install -r requirements.txt

collectstatic:
	python manage.py collectstatic --no-input --clear

migrate:
	python manage.py makemigrations
	python manage.py migrate

run:
	python manage.py runserver

create_po_files:
	django-admin makemessages -l es
	django-admin makemessages -l fr

create_mo_files:
	django-admin compilemessages
