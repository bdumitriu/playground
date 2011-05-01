@echo off

cd classes

C:\Utils\jdk1.5\bin\jar cfm cl.jar Manifest.txt *

move /Y cl.jar ..\

cd ..
