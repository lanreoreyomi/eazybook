FROM node:alpine AS build-stage
WORKDIR /app
COPY package*.json ./
RUN npm install
COPY . .

ARG VITE_APP_AUTHENTICATION_BASE_URL=http://authentication:9084
ARG VITE_APP_BOOK_CATALOGUE_URL=http://bookcatalogue:9189
ARG VITE_APP_WISHLIST_BASE_URL=http://wishlist:8017

ENV VITE_APP_AUTHENTICATION_BASE_URL=$VITE_APP_AUTHENTICATION_BASE_URL
ENV VITE_APP_BOOK_CATALOGUE_URL=$VITE_APP_BOOK_CATALOGUE_URL
ENV VITE_APP_WISHLIST_BASE_URL=$VITE_APP_WISHLIST_BASE_URL

RUN npm run build  # Outputs files to `/app/dist`

# Stage 2: Serve files with Nginx
FROM nginx:alpine
COPY --from=build-stage /app/dist /usr/share/nginx/html
#COPY nginx.conf /etc/nginx/conf.d/default.conf
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
