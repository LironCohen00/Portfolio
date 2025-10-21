from flask import Flask
from flask_restful import Resource, Api, reqparse
import json
import pymongo
from bson import json_util


app = Flask(__name__)  # initialize Flask
api = Api(app)  # create API
Client = pymongo.MongoClient("mongodb://mongo:27017")
db = Client["DataBase"]

def parse_json(data):
    return json.loads(json_util.dumps(data))
    
class DietsCollection():
    def __init__(self, DietCollection):
        self.dbDietsCol = DietCollection    
    
    def InsertDiet(self, args):
        document = {"name" : args["name"],
                    "cal" : args["cal"],
                    "sodium" : args["sodium"],
                    "sugar" : args["sugar"],}
        if not (self.dbDietsCol.find_one({"name" : document["name"]}) is None):
            return f"Diet with name {document['name']} already exists", 422
        else:
            self.dbDietsCol.insert_one(document)
            return f"Diet with name {document['name']} was created succesfully", 201
    
    def GetAllDiets(self):
        cursor = self.dbDietsCol.find({} , {"_id" : False})
        DocArr = []
        for document in cursor:
          DocArr.append(parse_json(document))
        return DocArr, 200
    
    def GetDiet(self, name):
        Doc = self.dbDietsCol.find_one({"name" : name} , {"_id" : False})
        if Doc is None:
            return f"Diet {name} not found", 404
        else:
            return parse_json(Doc), 200
        
DietsColl = DietsCollection(db["diets"])      

class Diets(Resource):
    global DietsColl

    def post(self):
        parser = reqparse.RequestParser()
        parser.add_argument('name', location='json', required=True, type=str)
        parser.add_argument('cal', location='json', required=True, type=float)
        parser.add_argument('sodium', location='json', required=True, type=float)
        parser.add_argument('sugar', location='json', required=True, type=float)
        args = parser.parse_args()
        return DietsColl.InsertDiet(args)
    
    def get(self):
        return DietsColl.GetAllDiets()

class DietsName(Resource):
    global DietsColl

    def get(self , name):
        return DietsColl.GetDiet(name)
    
if __name__ == '__main__':
    api.add_resource(Diets, '/diets')
    api.add_resource(DietsName, '/diets/<string:name>')
    app.run(host='0.0.0.0', port=8000, debug=True)
    
