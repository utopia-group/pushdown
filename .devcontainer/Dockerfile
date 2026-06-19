FROM apache/spark:4.0.0

# Switch to root to install packages
USER root

# Install essential utilities that VS Code needs
RUN apt-get update && apt-get install -y \
  curl \
  wget \
  ca-certificates \
  git \
  openssh-client \
  gnupg \
  procps \
  lsb-release \
  sudo \
  unzip \
  && rm -rf /var/lib/apt/lists/*

# Install sbt
RUN curl -fsSL https://github.com/sbt/sbt/releases/download/v1.10.7/sbt-1.10.7.tgz | tar xz -C /usr/local --strip-components=1

# Install opam and OCaml dependencies
RUN apt-get update && apt-get install -y \
  build-essential \
  m4 \
  pkg-config \
  opam \
  libgmp-dev \
  libssl-dev \
  zlib1g-dev \
  libbz2-dev \
  libreadline-dev \
  libsqlite3-dev \
  libncursesw5-dev \
  xz-utils \
  tk-dev \
  libxml2-dev \
  libxmlsec1-dev \
  libffi-dev \
  liblzma-dev \
  cmake \
  ninja-build \
  python3 \
  python3-pip \
  && rm -rf /var/lib/apt/lists/*

# Build and install Z3 4.15.3 from source
RUN wget https://github.com/Z3Prover/z3/archive/refs/tags/z3-4.15.3.tar.gz \
  && tar -xzf z3-4.15.3.tar.gz \
  && cd z3-z3-4.15.3 \
  && python3 scripts/mk_make.py --prefix=/usr/local \
  && cd build \
  && make -j$(nproc) \
  && make install \
  && cd ../.. \
  && rm -rf z3-4.15.3.tar.gz z3-z3-4.15.3

# Install CVC5 1.2.1
RUN wget https://github.com/cvc5/cvc5/releases/download/cvc5-1.2.1/cvc5-Linux-x86_64-static.zip \
  && unzip cvc5-Linux-x86_64-static.zip \
  && mv cvc5-Linux-x86_64-static/bin/cvc5 /usr/local/bin/cvc5 \
  && chmod +x /usr/local/bin/cvc5 \
  && rm -rf cvc5-Linux-x86_64-static.zip cvc5-Linux-x86_64-static

# Install Eldarica 2.1
RUN wget https://github.com/uuverifiers/eldarica/releases/download/v2.1/eldarica-bin-2.1.zip \
  && unzip eldarica-bin-2.1.zip \
  && mv eldarica /opt/eldarica \
  && ln -s /opt/eldarica/eld /usr/local/bin/eld \
  && chmod +x /opt/eldarica/eld \
  && rm eldarica-bin-2.1.zip

# Create vscode user (similar to devcontainer base)
RUN useradd -m -s /bin/bash -u 1000 vscode \
  && echo "vscode ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

# SPARK_HOME is already set in the base image to /opt/spark
ENV PATH="${PATH}:${SPARK_HOME}/bin"

USER vscode

# Initialize opam and install OCaml 4.14.1
RUN opam init -y --disable-sandboxing --compiler=4.14.1 \
  && eval $(opam env)

# Install OCaml dependencies from pusharoot.opam
RUN opam install -y \
  dune.3.19.1 \
  menhir.20240715 \
  core.v0.16.2 \
  core_unix.v0.16.0 \
  batteries.3.9.0 \
  ke.0.6 \
  re.1.13.2 \
  ppx_deriving.5.2.1 \
  ppx_jane.v0.16.0 \
  ppx_variants_conv.v0.16.0 \
  && eval $(opam env)

# Add opam environment to bashrc
RUN echo 'eval $(opam env)' >> ~/.bashrc

# Update PATH to include Eldarica (Z3 and CVC5 are already in /usr/local/bin)
ENV PATH="/opt/eldarica:${PATH}"

# Install Python packages globally
RUN pip3 install --upgrade pip \
  && pip3 install pandas numpy matplotlib
