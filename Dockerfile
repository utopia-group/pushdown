FROM apache/spark:4.0.0

USER root

# System packages
RUN apt-get update && apt-get install -y \
  curl wget ca-certificates git gnupg procps sudo unzip \
  build-essential m4 pkg-config opam libgmp-dev \
  cmake ninja-build python3 python3-pip python3-venv \
  python3-pyparsing python3-tomli \
  && rm -rf /var/lib/apt/lists/*

# sbt 1.11.2
RUN curl -fsSL https://github.com/sbt/sbt/releases/download/v1.11.2/sbt-1.11.2.tgz \
  | tar xz -C /usr/local --strip-components=1

# Z3 4.15.3
RUN wget -q https://github.com/Z3Prover/z3/archive/refs/tags/z3-4.15.3.tar.gz \
  && tar -xzf z3-4.15.3.tar.gz \
  && cd z3-z3-4.15.3 && python3 scripts/mk_make.py --prefix=/usr/local \
  && cd build && make -j$(nproc) && make install \
  && cd ../.. && rm -rf z3-4.15.3.tar.gz z3-z3-4.15.3

# CVC5 1.2.1 (built from source for portability)
RUN git clone --depth 1 --branch cvc5-1.2.1 https://github.com/cvc5/cvc5.git /tmp/cvc5 \
  && cd /tmp/cvc5 \
  && if [ "$(uname -m)" = "x86_64" ]; then \
  export CFLAGS="-march=x86-64 -mtune=generic"; \
  export CXXFLAGS="-march=x86-64 -mtune=generic"; \
  fi \
  && ./configure.sh production --auto-download --static --prefix=/usr/local \
  && cd build && make -j$(nproc) && make install \
  && cd / && rm -rf /tmp/cvc5

# Eldarica 2.2.1
RUN wget -q https://github.com/uuverifiers/eldarica/releases/download/v2.2.1/eldarica-bin-2.2.1.zip \
  && unzip -q eldarica-bin-2.2.1.zip \
  && mv eldarica-2.2.1 /opt/eldarica \
  && ln -s /opt/eldarica/eld /usr/local/bin/eld \
  && chmod +x /opt/eldarica/eld \
  && rm eldarica-bin-2.2.1.zip

# Python packages
RUN pip3 install --no-cache-dir pandas==2.3.3 numpy==2.0.2 matplotlib==3.9.4 pyarrow==21.0.0

# Create non-root user
RUN useradd -m -s /bin/bash -u 1000 reviewer \
  && echo "reviewer ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

ENV PATH="/opt/eldarica:${SPARK_HOME}/bin:${PATH}"

USER reviewer

# OCaml 4.14.1 + dependencies
RUN opam init -y --disable-sandboxing --compiler=4.14.1 \
  && eval $(opam env)
RUN opam install -y \
  dune.3.19.1 menhir.20240715 core.v0.16.2 core_unix.v0.16.0 \
  batteries.3.9.0 ke.0.6 re.1.13.2 \
  ppx_deriving.5.2.1 ppx_jane.v0.16.0 ppx_variants_conv.v0.16.0 \
  && eval $(opam env)
RUN echo 'eval $(opam env)' >> ~/.bashrc

# Copy project
WORKDIR /home/reviewer/pusharoo
COPY --chown=reviewer:reviewer . .

# Ensure sbt project config exists
RUN mkdir -p benchmarks/spark/project \
  && echo 'sbt.version=1.11.2' > benchmarks/spark/project/build.properties

# Build synthesizer
RUN bash -lc 'cd synthesizer && eval $(opam env) && dune build'

# Verify: reproduce all tables and figures from pre-computed data
RUN bash -lc 'python3 experiments.py'

ENTRYPOINT ["bash", "-l"]
