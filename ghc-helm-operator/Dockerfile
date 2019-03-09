FROM lachlanevenson/k8s-kubectl

RUN apk --update add ghc cabal postgresql-dev alpine-sdk linux-headers musl-dev gmp-dev zlib-dev coreutils

RUN cabal update

ENV VERSION 2.11.0

ENV BASE_URL="https://storage.googleapis.com/kubernetes-helm"
ENV TAR_FILE="helm-v${VERSION}-linux-amd64.tar.gz"

RUN curl -L ${BASE_URL}/${TAR_FILE} |tar xvz && \
    mv linux-amd64/helm /usr/bin/helm && \
    chmod +x /usr/bin/helm && \
    rm -rf linux-amd64

RUN helm init --client-only

#Add lakowske's helm repo.
RUN helm repo add lakowske https://lakowske.github.io/charts

#After adding the repo, update helm index.
RUN helm repo update

RUN mkdir /opt/server

# Add just the .cabal file to capture dependencies
COPY ./ghc-helm-operator.cabal /opt/server

WORKDIR /opt/server

RUN cabal install --only-dependencies -j4

COPY ./src /opt/server/src

COPY ./resources /opt/server/resources

RUN cabal build

RUN chmod +x /opt/server/dist/build/ghc-helm-operator/ghc-helm-operator

#ENTRYPOINT ls -al /opt/server/dist/build/ghc-helm-operator
ENTRYPOINT /opt/server/dist/build/ghc-helm-operator/ghc-helm-operator
#ENTRYPOINT whoami
