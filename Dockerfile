FROM toposoid/toposoid-core:0.4-SNAPSHOT

WORKDIR /app
ARG TARGET_BRANCH

ENV DEPLOYMENT=local
ENV _JAVA_OPTIONS="-Xms512m -Xmx4g"

RUN apt-get update \
&& apt-get install --no-install-recommends -y build-essential unzip zlib1g-dev cmake \
&& rm -rf /var/lib/apt/lists/* \
&& cd /tmp \
&& git clone https://github.com/toposoid/EvalMaxSAT \
&& mkdir EvalMaxSAT/build \
&& cd EvalMaxSAT/build \
&& cmake ..  \
&& make \
&& make install \
&& cd /app \
&& git clone https://github.com/toposoid/toposoid-sat-solver-web.git \
&& cd toposoid-sat-solver-web \
&& git fetch origin ${TARGET_BRANCH} \
&& git checkout ${TARGET_BRANCH} \
&& sbt playUpdateSecret 1> /dev/null \
&& sbt dist \
&& cd /app/toposoid-sat-solver-web/target/universal \
&& unzip -o toposoid-sat-solver-web-0.4-SNAPSHOT.zip


COPY ./docker-entrypoint.sh /app/
ENTRYPOINT ["/app/docker-entrypoint.sh"]

