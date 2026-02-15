# -----------------------------------------
# STAGE 1 - Build Lisp dependencies
# -----------------------------------------
FROM fukamachi/qlot AS build

WORKDIR /app

# アプリソースをコピー
COPY . /app

# qlot install 実行
RUN qlot install

# -----------------------------------------
# STAGE 2 - Runtime image
# -----------------------------------------
FROM fukamachi/sbcl

# 環境変数
ENV APP_ENV production
ENV CLACK_HANDLER woo
ENV PORT 80

# 必要パッケージをインストール
RUN set -x; \
  apt-get update && \
  apt-get -y install --no-install-recommends \
    gcc \
    libc6-dev \
    libev-dev \
    libyaml-dev && \
  rm -rf /var/lib/apt/lists/*

# アプリケーション配置
WORKDIR /app
COPY --from=build /app /app

# Lisp の依存を一度読み込んでコンパイルを進めておく
RUN set -x; \
  ros -l .qlot/setup.lisp \
    -e "(handler-bind ((error (lambda (e) (uiop:print-condition-backtrace e) (uiop:quit -1)))) \
          (ql:quickload (list :clack :woo)))"

RUN chmod +x /app/entrypoint.sh

ENTRYPOINT ["/app/entrypoint.sh"]