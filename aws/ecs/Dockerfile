FROM node:18-alpine

WORKDIR /app

# Install dependencies for wait-for-it
RUN apk add --no-cache bash

# Copy package files and install dependencies
COPY package*.json ./
RUN npm install

# Copy wait script
COPY wait-for-it.sh ./
RUN chmod +x wait-for-it.sh

# Copy application code
COPY . .

EXPOSE 3000

# Use wait-for-it script to wait for PostgreSQL to be ready
CMD ["npm", "start"]
