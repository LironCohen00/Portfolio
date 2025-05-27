FROM python:alpine3.17
WORKDIR ./app
COPY MealsAndDishesService.py .
RUN pip install flask
RUN pip install flask_restful
RUN pip install requests
RUN pip install pymongo
EXPOSE 8000
CMD ["python3", "MealsAndDishesService.py"]