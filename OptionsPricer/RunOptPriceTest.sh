#! /bin/bash -e

if [ "$4" == "" ]
then
	echo "PARAMS: OptionSpecFile NPaths NThreads NBlocks"
	exit 1
fi
OptionSpecFile="$1"
NPaths="$2"
NThreads="$3"
NBlocks="$4"

# Merge the NumEnv and OptionSpec:
InFile=/tmp/`date +%s`.in

cat > $InFile <<EOD
MCNumEnv1D {m_nPaths = $NPaths, m_timeStepY = 1.0e-3, m_rngSeed = 12345, m_rngImpl = Mersenne64, m_nParBlocks = $NBlocks, m_parEvalImpl = Eval}
EOD
cat $OptionSpecFile >> $InFile

# Run the Pricer:
/usr/bin/time ./OptPricerTest +RTS -N"$NThreads" < $InFile

# Delete the InFile:
rm -f $InFile

