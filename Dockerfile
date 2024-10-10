FROM rust:alpine AS build

WORKDIR /app
COPY . /app

RUN apk add --no-cache musl-dev \
  && cargo build --release

FROM alpine as runtime

COPY --from=build /app/target/release/ndc /
CMD ["./ndc"]
