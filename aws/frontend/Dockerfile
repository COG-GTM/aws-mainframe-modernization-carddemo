# syntax=docker/dockerfile:1
FROM node:20.20.2-alpine3.22 AS build
WORKDIR /app
COPY package.json package-lock.json .npmrc ./
RUN npm ci --no-audit --no-fund
COPY . .
ARG VITE_API_BASE_URL=""
ARG VITE_USE_MOCKS="false"
ENV VITE_API_BASE_URL=$VITE_API_BASE_URL VITE_USE_MOCKS=$VITE_USE_MOCKS
RUN npm run build

FROM nginx:1.28.0-alpine3.21
ENV API_UPSTREAM=http://services:8080
COPY nginx.conf /etc/nginx/templates/default.conf.template
COPY --from=build /app/dist /usr/share/nginx/html
EXPOSE 8080
HEALTHCHECK --interval=30s --timeout=3s CMD wget -qO- http://127.0.0.1:8080/healthz || exit 1
CMD ["nginx", "-g", "daemon off;"]
