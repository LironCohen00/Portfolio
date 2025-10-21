FROM python:alpine3.17
WORKDIR ./app
COPY DietService.py .
RUN pip install flask
RUN pip install flask_restful
RUN pip install pymongo
EXPOSE 8001
CMD ["python3", "DietService.py"]