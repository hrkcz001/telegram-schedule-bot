FROM haskell:9.2.7

COPY . /app

WORKDIR /app

RUN stack setup --install-ghc
RUN stack build

EXPOSE 8080
CMD stack exec telegram-delay-bot-exe
