sudo apt-get install build-essential libxslt-dev \
    zip unzip expat zlib1g-dev libssl-dev curl \
    libncurses5-dev git-core libexpat1-dev \
    htmldoc

curl -O https://raw.githubusercontent.com/yrashk/kerl/master/kerl
chmod a+x kerl
mv kerl /usr/bin
kerl list releases
kerl build 18.2 r18.2 # this takes a while
kerl install r18.2 /usr/lib/erlang
. /usr/lib/erlang/activate

cd /opt
git clone https://github.com/2600Hz/kazoo.git
cd kazoo
make
