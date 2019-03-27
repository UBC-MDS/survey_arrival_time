# Contributing
In this project, we will be adopting the Git version control workflow. Individual collaborator will have their own repository forked from the main master repository. Users can work locally by cloning their own repository and merging from the upstream master. To upload their work, changes can be pushed to their local and then merged into the master via pull requests.

### Example of a contributing workflow:

**Fork then clone repo **

```git clone <repo folder>```

**Update private repo branch**

`git remote add upstream <original_repo_URL>`<br>
`git pull upstream`
`git merge upstream/master`

**To submit changes to master branch**

`git add .`<br>
`git commit <meaningful message>`<br>
`git push`<br>
`submit pull request to main repo`
