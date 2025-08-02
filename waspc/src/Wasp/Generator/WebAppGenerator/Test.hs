module Wasp.Generator.WebAppGenerator.Test
  ( testWebApp,
  )
where

import StrongPath (Abs, Dir, Path', relfile, (</>))
import Wasp.Generator.WebAppGenerator.Common (webAppRootDirInProjectRootDir)
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)
import Wasp.Util.StrongPath (toPosixFilePath)

testWebApp :: [String] -> Path' Abs (Dir WaspProjectDir) -> J.Job
testWebApp args projectDir = do
  runNodeCommandAsJob projectDir "npx" (vitestCommand ++ args) J.WebApp
  where
    vitestCommand = ["vitest", "--config", toPosixFilePath viteConfigPath]
    viteConfigPath =
      dotWaspDirInWaspProjectDir
        </> generatedCodeDirInDotWaspDir
        </> webAppRootDirInProjectRootDir
        </> [relfile|vite.config.ts|]
