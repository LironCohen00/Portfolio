#!/usr/bin/env node
"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.convertTsvToSqlite = convertTsvToSqlite;
var fs = __importStar(require("fs"));
var path = __importStar(require("path"));
var better_sqlite3_1 = __importDefault(require("better-sqlite3"));
var TSV_FILE_PATH = path.join(process.cwd(), 'data_local_temp', 'opennutrition_foods.tsv');
var DB_FILE_PATH = path.join(process.cwd(), 'data_local', 'opennutrition_foods.db');
function convertTsvToSqlite() {
    try {
        if (!fs.existsSync(TSV_FILE_PATH)) {
            throw new Error("TSV file not found: ".concat(TSV_FILE_PATH));
        }
        console.log('Reading TSV file...');
        var tsvContent = fs.readFileSync(TSV_FILE_PATH, 'utf-8');
        var lines = tsvContent.trim().split('\n');
        if (lines.length === 0) {
            throw new Error('TSV file is empty');
        }
        var headers = lines[0].split('\t');
        var dataRows = lines.slice(1).map(function (line) { return line.split('\t'); });
        console.log("Found ".concat(headers.length, " columns and ").concat(dataRows.length, " data rows"));
        // Ensure the database directory exists
        var dbDir = path.dirname(DB_FILE_PATH);
        if (!fs.existsSync(dbDir)) {
            fs.mkdirSync(dbDir, { recursive: true });
            console.log("Created directory: ".concat(dbDir));
        }
        if (fs.existsSync(DB_FILE_PATH)) {
            fs.unlinkSync(DB_FILE_PATH);
            console.log('Removed existing database file');
        }
        var db = createDatabase();
        createTable(db, headers);
        insertData(db, headers, dataRows);
        db.close();
        console.log('Database connection closed');
        console.log("Successfully converted ".concat(TSV_FILE_PATH, " to ").concat(DB_FILE_PATH));
    }
    catch (error) {
        console.error('Error converting TSV to SQLite:', error);
        process.exit(1);
    }
}
function createDatabase() {
    console.log('Connected to SQLite database');
    return new better_sqlite3_1.default(DB_FILE_PATH);
}
function createTable(db, columns) {
    var columnDefinitions = columns.map(function (col) { return "\"".concat(col, "\" TEXT"); }).join(', ');
    var createTableSQL = "CREATE TABLE IF NOT EXISTS foods (".concat(columnDefinitions, ")");
    db.exec(createTableSQL);
    console.log('Created foods table');
}
function insertData(db, columns, rows) {
    var jsonColumns = [
        'alternate_names',
        'labels',
        'source',
        'nutrition_100g',
        'serving',
        'package_size',
        'ingredient_analysis',
    ];
    var columnSql = columns.map(function (col) {
        if (jsonColumns.includes(col)) {
            return "json(?) AS \"".concat(col, "\"");
        }
        else {
            return "? AS \"".concat(col, "\"");
        }
    }).join(', ');
    var insertSQL = "INSERT INTO foods SELECT ".concat(columnSql);
    var stmt = db.prepare(insertSQL);
    var insertMany = db.transaction(function (rows) {
        for (var _i = 0, rows_1 = rows; _i < rows_1.length; _i++) {
            var row = rows_1[_i];
            var rowToInsert = __spreadArray([], row, true);
            for (var _a = 0, jsonColumns_1 = jsonColumns; _a < jsonColumns_1.length; _a++) {
                var jsonCol = jsonColumns_1[_a];
                var colIndex = columns.indexOf(jsonCol);
                if (colIndex !== -1 && rowToInsert[colIndex] !== undefined && rowToInsert[colIndex] !== '') {
                    try {
                        // Parse and stringify to ensure valid JSON format
                        var parsed = JSON.parse(rowToInsert[colIndex]);
                        rowToInsert[colIndex] = JSON.stringify(parsed);
                    }
                    catch (e) {
                        console.warn("Warning: Could not parse JSON for column '".concat(jsonCol, "' with value '").concat(rowToInsert[colIndex], "'. Setting to NULL."));
                        rowToInsert[colIndex] = null;
                    }
                }
            }
            stmt.run(rowToInsert);
        }
    });
    insertMany(rows);
    console.log("Inserted ".concat(rows.length, " rows into database"));
}
if (import.meta.url === "file://".concat(process.argv[1])) {
    convertTsvToSqlite();
}
