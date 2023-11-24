module Mapa where


{-

andreia@andreia-Precision-3541:~$ l
'='         Desktop/      Downloads/   Public/      install.sh      snap/
 Aula1.hs   Documents/    Music/       Templates/   microsoft.gpg
 Cesium/    DonkeyKong/   Pictures/    Videos/      projeto/
andreia@andreia-Precision-3541:~$ ls
'='         Desktop      Downloads   Public      install.sh      snap
 Aula1.hs   Documents    Music       Templates   microsoft.gpg
 Cesium     DonkeyKong   Pictures    Videos      projeto
andreia@andreia-Precision-3541:~$ cd DonkeyKong
andreia@andreia-Precision-3541:~/DonkeyKong$ ls
2023li1g005
andreia@andreia-Precision-3541:~/DonkeyKong$ cd 2023li1g005
andreia@andreia-Precision-3541:~/DonkeyKong/2023li1g005$ ls
README.md  imagens  lib  primate-kong.cabal  src  test
andreia@andreia-Precision-3541:~/DonkeyKong/2023li1g005$ git status
On branch main
Your branch is up to date with 'origin/main'.

Untracked files:
  (use "git add <file>..." to include in what will be committed)
	lib/mapafinal.hs

nothing added to commit but untracked files present (use "git add" to track)
andreia@andreia-Precision-3541:~/DonkeyKong/2023li1g005$ git add lib/mapafinal.hs
andreia@andreia-Precision-3541:~/DonkeyKong/2023li1g005$ git status
On branch main
Your branch is up to date with 'origin/main'.

Changes to be committed:
  (use "git restore --staged <file>..." to unstage)
	new file:   lib/mapafinal.hs

andreia@andreia-Precision-3541:~/DonkeyKong/2023li1g005$ git commit -m "mapa adicionado"
[main 14d44c5] mapa adicionado
 1 file changed, 0 insertions(+), 0 deletions(-)
 create mode 100644 lib/mapafinal.hs
andreia@andreia-Precision-3541:~/DonkeyKong/2023li1g005$ git push
Username for 'https://gitlab.com': aacandreia13
Password for 'https://aacandreia13@gitlab.com': 
remote: HTTP Basic: Access denied. The provided password or token is incorrect or your account has 2FA enabled and you must use a personal access token instead of a password. See https://gitlab.com/help/topics/git/troubleshooting_git#error-on-git-fetch-http-basic-access-denied
fatal: Authentication failed for 'https://gitlab.com/uminho-di/li1/2324/projetos/2023li1g005.git/'
andreia@andreia-Precision-3541:~/DonkeyKong/2023li1g005$ git push
Username for 'https://gitlab.com': aacandreia13
Password for 'https://aacandreia13@gitlab.com': 
Enumerating objects: 6, done.
Counting objects: 100% (6/6), done.
Delta compression using up to 12 threads
Compressing objects: 100% (3/3), done.
Writing objects: 100% (4/4), 333 bytes | 333.00 KiB/s, done.
Total 4 (delta 2), reused 0 (delta 0), pack-reused 0
To https://gitlab.com/uminho-di/li1/2324/projetos/2023li1g005.git
   7bc26b3..14d44c5  main -> main
andreia@andreia-Precision-3541:~/DonkeyKong/2023li1g005$ 
-}