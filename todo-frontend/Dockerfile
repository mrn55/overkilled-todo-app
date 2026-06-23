# Build stage
FROM node:22.11.0-alpine3.20 AS builder
WORKDIR /app
COPY package.json package-lock.json ./
RUN npm ci
COPY . .
RUN npm run build

# Serve with NGINX
FROM nginxinc/nginx-unprivileged:1.27.3-alpine

LABEL org.opencontainers.image.title="Overkilled Todo Frontend" \
      org.opencontainers.image.description="React frontend for the Overkill(ed) Todo App" \
      org.opencontainers.image.source="https://github.com/mrn55/overkilled-todo-app" \
      org.opencontainers.image.version="0.1.0" \
      org.opencontainers.image.licenses="MIT"

COPY --from=builder /app/dist /usr/share/nginx/html
COPY nginx.conf /etc/nginx/conf.d/default.conf

EXPOSE 8080
CMD ["nginx", "-g", "daemon off;"]
