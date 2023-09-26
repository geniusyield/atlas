#!/bin/bash
set -Eeuo pipefail
echo "===============[ PRIVATE NETWORK TESTS ]======================"
echo " Started at $(date --iso-8601=seconds --utc) (${BASH_SOURCE[0]})."
git clone https://github.com/geniusyield/cardano-private-testnet-setup.git
cd cardano-private-testnet-setup
git checkout geniusyield
cd ..
TESTNET="./cardano-private-testnet-setup"
LOGFILE="./logs/privnet.log"
echo " TESTNET: $TESTNET"
echo " LOGFILE: $LOGFILE"
echo " CI RUN : ${CI:=false}"
echo "========================[START PRIVNET]======================="
mkdir -p logs
touch $LOGFILE
((cd $TESTNET && ./scripts/automate.sh) | tee $LOGFILE) &

until [[ $(tail -n 1 $LOGFILE) =~ "Congrats! Your network is ready for use!" ]]
do
    sleep 5
done

echo "================================================================"
echo " [OK] Private testnet ready."
echo "=======================[DOWNLOAD KUPO]=========================="
curl -sL https://github.com/CardanoSolutions/kupo/releases/download/v2.6/kupo-2.6.1-amd64-Linux.tar.gz > ./kupo.tar.gz
tar xf ./kupo.tar.gz bin/kupo --warning=no-unknown-keyword || tar xf ./kupo.tar.gz bin/kupo  # https://stackoverflow.com/a/76203961/11183512
mv ./bin/kupo ./kupo
rm -rf ./bin ./kupo.tar.gz
chmod a+x ./kupo
echo "========================[START KUPO]============================"
(./kupo \
  --node-socket $TESTNET/private-testnet/node-spo1/node.sock \
  --node-config $TESTNET/private-testnet/configuration.yaml \
  --since origin \
  --match "*" \
  --prune-utxo \
  --in-memory &>/dev/null) &

echo " -> Starting geniusyield-privnet-tests..."
echo "=====================[RUN PRIVNET TESTS]========================"
echo
set -x
KUPO_URL=http://localhost:1442 GENIUSYIELD_PRIVNET_DIR=$TESTNET/private-testnet cabal run atlas-privnet-tests -- -j1 --hide-successes
set +x
echo
if [ "$CI" = false ]
then
  echo "===================[KILL PRIVNET AND KUPO]===================="
  echo "[INFO] Local, Non-CI run -> Kill the private testnet."
  set -x
  trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT
  set +x
  rm -rf ./cardano-private-testnet-setup/ ./kupo ./logs
fi
echo "============================[SUMMARY]==========================="
echo
echo " [DONE] Executed: ${BASH_SOURCE[0]} ($(date --iso-8601=seconds --utc))."
echo
echo "================================================================"
