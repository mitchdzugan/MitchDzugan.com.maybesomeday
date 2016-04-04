printf "Building Client...\n" \
  && stack build --stack-yaml=client/stack.yaml \
  && printf "\nCopying commpiled js and building server...\n" \
  && cp client/.stack-work/dist/x86_64-linux/Cabal-1.22.4.0_ghcjs/build/client/client.jsexe/*.js ./server/static/ \
  && stack build server --stack-yaml=server/stack.yaml \
  && printf "\nRunning server...\n" \
  && (killall server-exe; stack exec server-exe --stack-yaml=server/stack.yaml)