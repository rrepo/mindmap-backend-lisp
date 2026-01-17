#!/bin/bash

# .env から BACKEND_TOKEN_SECRET を取得
TOKEN=$(grep BACKEND_TOKEN_SECRET .env | cut -d '=' -f2-)
echo "Using token!!!!!!!: $TOKEN"

# curl -X POST "http://localhost:5000/create-map" \
# -H "Content-Type: application/json" \
# -H "x-service-token: $TOKEN" \
# -d '{
#       "uid": "OtUU0vC1QJSUXUSDJjVV3Zu4v1E3",
#       "title": "manual test map",
#       "visibility": "public"
# }'

# API_URL="http://localhost:5000/create-map"

# if [ -z "$TOKEN" ]; then
#     echo "ERROR: TOKEN environment variable is not set"
#     exit 1
# fi

# for i in $(seq 1 30); do
#     echo "Creating map $i..."
    
#     curl -s -X POST "$API_URL" \
#     -H "Content-Type: application/json" \
#     -H "x-service-token: $TOKEN" \
#     -d '{
#           "uid": "OtUU0vC1QJSUXUSDJjVV3Zu4v1E3",
#           "title": "manual test map '"$i"'",
#           "visibility": "public"
#     }'
    
#     echo
# done

# curl -X POST http://localhost:5000/create-node \
#   -H "Content-Type: application/json" \
#   -H "x-service-token: $TOKEN" \
#   -d '{
#     "map-id": 4,
#     "parent-id": null,
#     "uid": "OtUU0vC1QJSUXUSDJjVV3Zu4v1E3",
#     "content": "manual test node"
#   }'

# API にアクセス
# curl -X POST http://localhost:5000/create-map-invitation \
#   -H "Content-Type: application/json" \
#   -H "x-service-token: $TOKEN" \
#   -d '{"uid":"OtUU0vC1QJSUXUSDJjVV3Zu4v1E3","map-id":"2"}'

# curl -X POST http://localhost:5000/get-maps-by-uid?id=OtUU0vC1QJSUXUSDJjVV3Zu4v1E3 \
#   -H "Content-Type: application/json" \
#   -H "x-service-token: $TOKEN" \

# curl -X POST http://localhost:5000/get-map-details?id=c6e590f2-1d09-4cd7-a822-112cefa0a5b3 \
#   -H "Content-Type: application/json" \
#   -H "x-service-token: $TOKEN" \

# curl -X POST http://localhost:5000/create-map-member \
#   -H "Content-Type: application/json" \
#   -H "x-service-token: $TOKEN" \
#   -d '{"uid":"OtUU0vC1QJSUXUSDJjVV3Zu4v1E3","token":"gAMlfx621JJJFGxQyFBa2Ut9qO4tS16mMds19IQqW2Q."}'

# curl -X POST http://localhost:5000/all-nodes \  -H "Content-Type: application/json" \  -H "x-service-token: $TOKEN" \

# curl -X POST http://localhost:5000/delete-map-member \
#   -H "Content-Type: application/json" \
#   -H "x-service-token: $TOKEN" \
#   -d '{"uid":"toCo2nB5VHVEzlfDy3Gq3am7Wpf2","map-id":"2"}'

# curl -X POST http://localhost:5000/all-map-members \
#   -H "Content-Type: application/json" \
#   -H "x-service-token: $TOKEN" \

# curl -X POST http://localhost:5000/all-map-members \
#   -H "Content-Type: application/json" \
#   -H "x-service-token: $TOKEN" \

# curl -X POST http://localhost:5000/get-map-members-by-map-id?=2 \
#   -H "Content-Type: application/json" \
#   -H "x-service-token: $TOKEN" \

# curl -X POST http://localhost:5000/get-maps-by-uid?id=OtUU0vC1QJSUXUSDJjVV3Zu4v1E3 \
#   -H "Content-Type: application/json" \
#   -H "x-service-token: $TOKEN" \

curl -X POST http://localhost:5000/get-latest-public-maps \
  -H "Content-Type: application/json" \
  -H "x-service-token: $TOKEN" \