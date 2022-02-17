#! /bin/bash -e

if [ "$3" == "" ]
then
	echo "PARAMS: NPaths NThreads NBlocks"
	exit 1
fi
NPaths="$1"
NThreads="$2"
NBlocks="$3"

exec /usr/bin/time ./OptPricerTest +RTS -N"$NThreads" <<EOD
MCNumEnv1D {m_nPaths = $NPaths, m_timeStepY = 1.0e-3, m_rngSeed = 12345, m_rngImpl = Mersenne64, m_nParBlocks = $NBlocks, m_parEvalImpl = Eval}
EOD
