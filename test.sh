#!/bin/bash

# .env から BACKEND_TOKEN_SECRET を取得
TOKEN=$(grep BACKEND_TOKEN_SECRET .env | cut -d '=' -f2-)
echo "Using token!!!!!!!: $TOKEN"

# API にアクセス
curl -X GET http://localhost:5000/get-map?id=1 \
  -H "Content-Type: application/json" \
  -H "x-service-token: $TOKEN"