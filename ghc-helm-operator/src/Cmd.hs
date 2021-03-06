module Cmd
  where

import System.Process

data Shell a = Shell a deriving (Show)

instance Functor Shell where
  fmap f (Shell a) = Shell (f a)

instance Applicative Shell where
  pure = Shell
  (Shell f) <*> shellItem = fmap f shellItem

instance Monad Shell where
  return x = Shell x
  Shell x >>= f = f x

  

toProc :: Shell String -> CreateProcess
toProc (Shell a) = shell a

readShell :: Shell String -> IO String
readShell s = do
  (exitCode, sout, serr) <- readCreateProcessWithExitCode (toProc s) ""
  case serr == "" of
    True -> return sout
    False -> return serr


copyDebian :: Shell String
copyDebian = Shell "scp -r debian/ user@remote-host.com:~/"

showHome :: Shell String
showHome = Shell "ls /Users/home"

toNamespace :: String -> String -> Shell String
toNamespace namespace command = Shell (command ++ " --namespace " ++ namespace)

outputYaml :: String -> Shell String
outputYaml command = Shell (command ++ " -o yaml")

getNodePort :: String -> String -> Shell String
getNodePort namespace configMap = Shell ("kubectl get configmap --namespace "++namespace++" "++configMap++" -o jsonpath=\"{.data.port}\"")

getPort :: String -> Shell String
getPort release = Shell ("kubectl get service --namespace default " ++ release ++ "-postgresql -o jsonpath=\"{.spec.ports[0].port}\"")

getUser :: String -> String -> Shell String
getUser namespace configMap = Shell ("kubectl get configmap --namespace "++namespace++" "++configMap++" -o jsonpath=\"{.data.username}\"")

getPassword :: String -> String -> Shell String
getPassword namespace secret = Shell ("kubectl get secret --namespace "++namespace++" "++ secret ++ " -o jsonpath=\"{.data.password}\" | base64 --decode;")

getDB :: String -> String -> Shell String
getDB namespace configMap = Shell ("kubectl get configmap --namespace "++namespace++" "++configMap++" -o jsonpath=\"{.data.database}\"")

getHost :: String -> String -> Shell String
getHost namespace configMap = Shell ("kubectl get configmap --namespace "++namespace++" "++configMap++" -o jsonpath=\"{.data.host}\"")

getResource :: String -> Shell String
getResource resource = do
  return ("kubectl get " ++ resource)

getNamespacedResource resource namespace = getResource resource >>= toNamespace namespace

getNamespacedResourceYaml resource namespace = getResource resource >>= toNamespace namespace >>= outputYaml

fromLiteral :: [(String, String)] -> String -> String
fromLiteral (x:xs) line = fromLiteral xs (line ++ " --from-literal="++(fst x)++"="++(snd x))
fromLiteral [] line = line



createConfigMap :: String -> [(String, String)] -> Shell String
createConfigMap name literals = Shell $ fromLiteral literals ("kubectl create configmap " ++ name)

deleteConfigMap :: String -> Shell String
deleteConfigMap name = Shell ("kubectl delete configmap "++name)

createSecret :: String -> [(String, String)] -> Shell String
createSecret name literals = Shell $ fromLiteral literals ("kubectl create secret generic " ++ name)

deleteSecret ::  String -> Shell String
deleteSecret name = Shell ("kubectl delete secret "++name)

createNamespace :: String -> Shell String
createNamespace name = Shell ("kubectl create namespace "++name)

echo :: String -> Shell String
echo = (\x -> Shell ("echo \"" ++ x ++ "\""))

export :: String -> String -> Shell String
export key val = Shell ("export "++key++"=\""++val++"\"")

next :: Shell String -> (String -> Shell String)
next (Shell a) = (\x -> Shell (x++"; " ++ a))

helmList :: Shell String
helmList = Shell "helm list"

helmListName :: String -> Shell String
helmListName release = Shell ("helm list -q " ++ release)

helmContains :: String -> IO Bool
helmContains release = do
  name <- (readShell $ helmListName release)
  return (length name /= 0)

helmFailed :: String -> IO Bool
helmFailed release = do
  name <- (readShell $ Shell ("helm list --failed -q " ++ release))
  return (length name /= 0)

helmSuccess :: String -> IO Bool
helmSuccess release = do
  exists <- helmContains release
  failed <- helmFailed release
  putStrLn $ show exists
  putStrLn $ show failed
  return (exists && (not failed))
  
helmInstall :: String -> String -> Shell String
helmInstall chart args = Shell ("helm install " ++ chart ++ " " ++ args)

helmUpgrade :: String -> String -> String -> Shell String
helmUpgrade chart release args = Shell ("helm upgrade -i " ++ release ++ " " ++ chart ++ " " ++ args)

helmDelete :: String -> Shell String
helmDelete release = Shell ("helm del --purge " ++ release)

helmAddLakowskeRepo :: Shell String
helmAddLakowskeRepo = Shell ("helm repo add lakowske https://lakowske.github.io/charts")

helmUpdateRepos :: Shell String
helmUpdateRepos = Shell ("helm repo update")


passwd :: String -> String -> Shell String
passwd namespace secret = getPassword namespace secret >>= echo

doStuff :: IO ()
doStuff = do
  _ <- readShell pwd
  _ <- readShell helmList
  return ()

dockerBuild :: Shell String  
dockerBuild = Shell ("eval $(minikube docker-env) ; docker build -t dbme:v15 . ; docker tag dbme:v15 registry.gke.st81ess.com/dbme:v15 ; docker push registry.gke.st81ess.com/dbme:v15")

pwd :: Shell String
pwd = Shell ("pwd")
