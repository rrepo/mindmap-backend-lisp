#!/bin/bash

# .env から BACKEND_TOKEN_SECRET を取得
TOKEN=$(grep BACKEND_TOKEN_SECRET .env | cut -d '=' -f2-)
echo "Using token!!!!!!!: $TOKEN"

# API にアクセス
# curl -X POST http://localhost:5000/create-user \
#   -H "Content-Type: application/json" \
#   -H "x-service-token: $TOKEN" \
#   -d '{"uid":"2223","name":"33","img":"test.png"}'

curl -X POST http://localhost:5000/all-users \
  -H "Content-Type: application/json" \
  -H "x-service-token: $TOKEN" \
