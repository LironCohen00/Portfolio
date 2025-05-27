from flask import Flask
from flask_restful import Resource, Api, reqparse
import requests
import pymongo
from bson import json_util
import json

app = Flask(__name__)  # initialize Flask
api = Api(app)  # create API
Client = pymongo.MongoClient("mongodb://mongo:27017")
db = Client["DataBase"]

def parse_json(data):
    return json.loads(json_util.dumps(data))


class dish():  
    def api_call(self, query):
        api_url = 'https://api.api-ninjas.com/v1/nutrition?query={}'.format(query)
        query = query
        api_url = 'https://api.api-ninjas.com/v1/nutrition?query={}'.format(query)
        response = requests.get(api_url, headers={'X-Api-Key': 'JTNwmSBjEi97fQhVG0cWuQ==FgDuQk8zsrFueEyU'})
        if (response.text == "[]"):
            return response.json() , 422, -3
        elif (response.status_code == requests.codes.ok):
            return response.json(), 200, 0  # third value is irrelavent for now
        else:
            print("Error:", response.status_code, response.text)
            return response.json() , 504, -4 

    def __init__(self, query, id_num):
        self.dish_json = {
                "name" : query,
                "ID" : id_num,
                "cal" : 0,
                "size" : 0,
                "sodium" : 0,
                "sugar" : 0
                }

        response_json, code, value = self.api_call(query)
        
        for i in range(len(response_json)):
            self.dish_json["cal"] += response_json[0]["calories"]
            self.dish_json["size"] += response_json[0]["serving_size_g"]
            self.dish_json["sodium"] += response_json[0]["sodium_mg"]
            self.dish_json["sugar"] += response_json[0]["sugar_g"]

class dishes_collection():
    def __init__(self, dbDishesCol, dbMealsCol):
        self.meals = dbMealsCol
        self.dishes = dbDishesCol
        self.dishes.insert_one({"ID" : 0,
                                "CurrentDishID" : 1})


    def insert_dish(self, dish_name):

        if not (self.dishes.find_one({"name" : dish_name}) is None):
            print("dish already exists")
            return -2, 422

        currentID = self.dishes.find_one({"ID" : 0})["CurrentDishID"]
        new_dish = dish(dish_name, currentID)
        response, code, value = new_dish.api_call(dish_name)
        if code == 422:
            return -3, 422
        else:
            self.dishes.insert_one(new_dish.dish_json)
            currentID += 1
            self.dishes.update_one({"ID" : 0} , {"$set" : {"CurrentDishID" : currentID}})
            return currentID - 1, 201
    
    def get_dish_by_id(self, id_to_get):
        Doc = self.dishes.find_one({"ID" : id_to_get}, {"_id" : False})
        if (Doc is None):
            return -5, 404
        else:
            return parse_json(Doc), 200
        
    def get_dish_by_name(self, name_to_get):
        Doc = self.dishes.find_one({"name" : name_to_get}, {"_id" : False})
        if (Doc is None):
            return -5, 404
        else:
            return parse_json(Doc), 200
        

    def delete_dish_by_id(self, id_to_delete, meals):
        Doc = self.dishes.find_one({"ID" : int(id_to_delete)})
        if (Doc is None):
            return -5,404
        else:
            self.meals.update_many({"appetizer" : int(id_to_delete)} , {"$set" : {"appetizer" : None}})
            self.meals.update_many({"main" : int(id_to_delete)} , {"$set" : {"main" : None}})
            self.meals.update_many({"dessert" : int(id_to_delete)} , {"$set" : {"dessert" : None}})
            self.dishes.delete_one({"ID" : int(id_to_delete)})
            return id_to_delete, 200
        
    def delete_dish_by_name(self, name_to_delete, meals):
        Doc = self.dishes.find_one({"name" : name_to_delete})
        if (Doc is None):
            return -5,404
        else:
            id = Doc["ID"]
            self.meals.update_many({"appetizer" : id} , {"$set" : {"appetizer" : None}})
            self.meals.update_many({"main" : id} , {"$set" : {"main" : None}})
            self.meals.update_many({"dessert" : id} , {"$set" : {"dessert" : None}})
            self.dishes.delete_one({"ID" : id})
            return id, 200
        
class meals_collection():
    def __init__(self, dbMealsCol, dbDishesCol):
        self.meals = dbMealsCol
        self.meals.insert_one({"ID" : 0,
                                "CurrentMealID" : 1})
        self.dishes = dbDishesCol

    def add_meal(self, meal_name, starter_ID, main_ID, dessert_ID):
        StarterDish = self.dishes.find_one({"ID" : starter_ID})
        MainDish = self.dishes.find_one({"ID" : main_ID})
        DessertDish = self.dishes.find_one({"ID" : dessert_ID})
        
        if ((StarterDish is None) or (MainDish is None) or (DessertDish is None)):
            return -6, 422

        if not (self.meals.find_one({"name" : meal_name}) is None) : # if meal with this name already exists
            return -2, 422
     
        cal = StarterDish["cal"] + MainDish["cal"] + DessertDish["cal"]
        sodium = StarterDish["sodium"] + MainDish["sodium"] + DessertDish["sodium"]
        sugar = StarterDish["sugar"] + MainDish["sugar"] + DessertDish["sugar"]
        
        currentID = self.meals.find_one({"ID" : 0})["CurrentMealID"]
        self.meals.insert_one({"name" : meal_name,
                        "ID" : currentID,
                        "appetizer" : starter_ID,
                        "main" : main_ID,
                        "dessert" : dessert_ID, 
                        "cal" : cal ,
                        "sodium" : sodium,
                        "sugar" : sugar
                        })
        currentID += 1
        self.meals.update_one({"ID" : 0} , {"$set" : {"CurrentMealID" : currentID}})
        return currentID - 1, 201

    def get_meal_by_id(self, meal_id):
        Doc = self.meals.find_one({"ID" : meal_id}, {"_id" : False})
        if (Doc is None):
            return -5, 404
        else:
            return parse_json(Doc), 200
        
    def get_meal_by_name(self, meal_name):
        Doc = self.meals.find_one({"name" : meal_name}, {"_id" : False})
        if (Doc is None):
            return -5, 404
        else:
            return parse_json(Doc), 200
    
    def delete_meal_by_id(self, meal_id):
        Doc = self.meals.find_one({"ID" : meal_id})
        if (Doc is None):
            return -5,404
        else:
            self.meals.delete_one({"ID" : meal_id})
            return meal_id, 200
    
    def delete_meal_by_name(self, name_to_delete):
        Doc = self.meals.find_one({"name" : name_to_delete})
        if (Doc is None):
            return -5,404
        else:
            id = Doc["ID"]
            self.meals.delete_one({"name" : name_to_delete})
            return id, 200
        
    def put_update(self, meal_name, starter_ID, main_ID, dessert_ID, meal_id):
        StarterDish = self.dishes.find_one({"ID" : starter_ID})
        MainDish = self.dishes.find_one({"ID" : main_ID})
        DessertDish = self.dishes.find_one({"ID" : dessert_ID})
        
        if ((StarterDish is None) or (MainDish is None) or (DessertDish is None)):
            return -5, 422
        else:
            self.meals.update_one( {"ID" : meal_id} , { "$set" : {"name" : meal_name,
                                                                "appetizer" : starter_ID,
                                                                "main" : main_ID,
                                                                "dessert" : dessert_ID }})
            return meal_id, 200
            

dishes_col = dishes_collection(db["dishes"], db["meals"])
meals_col = meals_collection(db["meals"], db["dishes"])

class dishes(Resource):
    global dishes_col
    global meals_col

    def post(self):
        api_url = 'https://api.api-ninjas.com/v1/nutrition?query={}'.format(dishes_col.dishes.find_one({"ID" : 0})["CurrentDishID"])
        response = requests.get(api_url, headers={'X-Api-Key': 'JTNwmSBjEi97fQhVG0cWuQ==FgDuQk8zsrFueEyU'})
        if (response.headers.get('content-type') != 'application/json'):
            return 0, 415
        parser = reqparse.RequestParser()
        parser.add_argument('name', location='json', required = False)
        args = parser.parse_args()
        if args["name"] == None:
            return -1, 422
        else:
            return dishes_col.insert_dish(args["name"])
        
    def get(self):
        cursor = dishes_col.dishes.find({"ID" : {"$gte" : 1}} , {"_id" : False})
        DocArr = []
        for document in cursor:
          DocArr.append(parse_json(document))
        return DocArr, 200


class meals(Resource):
    global meals_col

    def post(self):
        api_url = 'https://api.api-ninjas.com/v1/nutrition?query={}'.format(dishes_col.dishes.find_one({"ID" : 0})["CurrentDishID"])
        response = requests.get(api_url, headers={'X-Api-Key': 'JTNwmSBjEi97fQhVG0cWuQ==FgDuQk8zsrFueEyU'})
        if (response.headers.get('content-type') != 'application/json'):
            return 0, 415
        parser = reqparse.RequestParser()
        parser.add_argument('name', location='json', required=False)
        parser.add_argument('appetizer', location='json', required=False)
        parser.add_argument('main', location='json', required=False)
        parser.add_argument('dessert', location='json', required=False)
        args = parser.parse_args()
        if ((args["name"] == None) or (args["appetizer"] == None) or (args["main"] == None) or (args["dessert"] == None)):
            return -1, 422
        else:
            appetizer_int = int(args["appetizer"])
            main_int = int(args["main"])
            dessert_int = int(args["dessert"])
            return meals_col.add_meal(args["name"],appetizer_int, main_int, dessert_int)

    def get(self):
        try: 
            parser = reqparse.RequestParser()
            parser.add_argument('diet', type=str, location="args" , required=True)
            args = parser.parse_args()
            URL = "http://diet_svr:8000/diets/" + args["diet"]  
            response = requests.get(url=URL)
            responseJSON = response.json()
            cursor = meals_col.meals.find({ '$and' : [ {"cal" : {"$lte" : responseJSON["cal"]}} , {"sodium" : {"$lte" : responseJSON["sodium"]}}, {"sugar" : {"$lte" : responseJSON["sugar"]}}]} , {"_id" : False})
            DocArr = []
            for document in cursor:
                DocArr.append(parse_json(document))
            return DocArr, 200
        except Exception as e:
            cursor = dishes_col.meals.find({"ID" : {"$gte" : 1}} , {"_id" : False})
            DocArr = []
            for document in cursor:
                DocArr.append(parse_json(document))
            return DocArr, 200
        
        
class dishKey(Resource):
    global meals_col

    def get(self, key):
        return dishes_col.get_dish_by_id(key)
    
    def delete(self, key):
        return dishes_col.delete_dish_by_id(key, meals_col)

class dishName(Resource):
    global meals_col

    def get(self, name):
        return dishes_col.get_dish_by_name(name)
    
    def delete(self, name):
        return dishes_col.delete_dish_by_name(name, meals_col)

class mealKey(Resource):
    global dishes_col

    def get(self, key):
        return meals_col.get_meal_by_id(key)
    
    def delete(self, key):
        return meals_col.delete_meal_by_id(key)
    
    def put(self, key):
        parser = reqparse.RequestParser()
        parser.add_argument('name', location='json', required=True)
        parser.add_argument('appetizer', location='json', required=True)
        parser.add_argument('main', location='json', required=True)
        parser.add_argument('dessert', location='json', required=True)
        args = parser.parse_args()
        appetizer_int = int(args["appetizer"])
        main_int = int(args["main"])
        dessert_int = int(args["dessert"])
        return meals_col.put_update(args["name"], appetizer_int, main_int, dessert_int, key)

class mealName(Resource):
    global dishes_col

    def get(self, name):
        return meals_col.get_meal_by_name(name)
    
    def delete(self, name):
        return meals_col.delete_meal_by_name(name)

if __name__ == '__main__':
    api.add_resource(dishes, '/dishes')
    api.add_resource(meals, '/meals')
    api.add_resource(dishKey, '/dishes/<int:key>')
    api.add_resource(dishName, '/dishes/<string:name>')
    api.add_resource(mealKey, '/meals/<int:key>')
    api.add_resource(mealName, '/meals/<string:name>')
    app.run(host='0.0.0.0', port=8000, debug=True)
    