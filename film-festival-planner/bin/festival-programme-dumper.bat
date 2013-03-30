@echo off

setlocal EnableDelayedExpansion

set WEBINF=..\target\film-festival-planner-1.0-SNAPSHOT_exploded.war\WEB-INF
set M2_REPO_HOME=c:\Users\bdumitriu\.m2\repository
set MYSQL_CONNECTOR=C:\Users\bdumitriu\jboss-as-7.1.1.Final\modules\com\mysql\main\mysql-connector-java-5.1.22-bin.jar

set CLASSPATH=
set CLASSPATH=%CLASSPATH%;%WEBINF%
set CLASSPATH=%CLASSPATH%;%WEBINF%\classes
set CLASSPATH=%CLASSPATH%;%WEBINF%\classes\META-INF
set CLASSPATH=%CLASSPATH%;%WEBINF%\lib\*
set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\scala-lang\scala-library\2.10.0\scala-library-2.10.0.jar
set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\hibernate\javax\persistence\hibernate-jpa-2.0-api\1.0.1.Final\hibernate-jpa-2.0-api-1.0.1.Final.jar
set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\hibernate\hibernate-entitymanager\4.1.9.Final\hibernate-entitymanager-4.1.9.Final.jar
set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\jboss\logging\jboss-logging\3.1.0.GA\jboss-logging-3.1.0.GA.jar
set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\javassist\javassist\3.17.1-GA\javassist-3.17.1-GA.jar
set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\jboss\spec\javax\transaction\jboss-transaction-api_1.1_spec\1.0.1.Final\jboss-transaction-api_1.1_spec-1.0.1.Final.jar
set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\dom4j\dom4j\1.6.1\dom4j-1.6.1.jar
set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\hibernate\hibernate-core\4.1.9.Final\hibernate-core-4.1.9.Final.jar
set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\antlr\antlr\2.7.7\antlr-2.7.7.jar
set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\hibernate\common\hibernate-commons-annotations\4.0.1.Final\hibernate-commons-annotations-4.0.1.Final.jar
set CLASSPATH=%CLASSPATH%;%MYSQL_CONNECTOR%
rem set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\jboss\weld\weld-se\1.0.1-Final\weld-se-1.0.1-Final.jar
rem set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\slf4j\slf4j-simple\1.5.10\slf4j-simple-1.5.10.jar
rem set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\slf4j\slf4j-api\1.5.10\slf4j-api-1.5.10.jar
set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\jboss\spec\javax\ejb\jboss-ejb-api_3.1_spec\1.0.2.Final\jboss-ejb-api_3.1_spec-1.0.2.Final.jar
set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\jboss\spec\javax\faces\jboss-jsf-api_2.1_spec\2.0.2.Final\jboss-jsf-api_2.1_spec-2.0.2.Final.jar
set CLASSPATH=%CLASSPATH%;%M2_REPO_HOME%\org\jboss\spec\javax\servlet\jboss-servlet-api_3.0_spec\1.0.1.Final\jboss-servlet-api_3.0_spec-1.0.1.Final.jar

java -classpath "%CLASSPATH%" org.ffplanner.scripts.FestivalProgrammeDumper %*

endlocal
