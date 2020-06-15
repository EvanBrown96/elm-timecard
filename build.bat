@ECHO OFF
pushd .
cd %~dp0
elm make src\Main.elm
popd
