# :wave: Demo Project

To complete this demo project:

1. read the contents of this README to learn more about git and GitHub.
1. add an .R file to this repo that contains just one line of code that creates a character vector of the word `"cat"` repeated eight times.
1. modify this `README.md` file by adding a new line at the very end that expresses your current feeling towards git and GitHub. You can answer either in text or (preferably) with an emoji.


## The Four Steps of Assignments through GitHub

You can submit your work for your Projects by following these seven steps. After reading through them here, watch our screencast video that demonstrates the workflow.

1. *Accept the GitHub Classroom assignment and tag your name*. This will copy an assignment template repo into our GitHub classroom that only you and your GSI will have access to. Tagging your name helps the GSI identify your work.

   If you're reading this README.md, you've already done this step!
1. *Clone the repo locally*. Cloning is the easiest way to copy a repository from GitHub to your own computer in a way that makes it easy to sync the two. To do this, run the following command at the command line on your laptop in the directory where you would like the repo to appear.

   `git clone <url of assignment>`

   You can copy the URL of the assignment by clicking the green "Code" button at the top of the repo on GitHub.

   After running the clone command, you will see that it has copied the files in this respository to your computer and also initialized it as a git repo.
1. *Complete the assignment*. This means working in the files in your local repo, adding text and R code and any other files that you like in order to complete the assignment.
1. *Commit and Push your work by the deadline*. To send your added and changed files back to your repository on GitHub, you'll need to *stage* the changes (using `git add`), commit those changes with a message (`git commit -m "something descriptive"`), then push those commits (`git push -u origin main`).

It's best practice to make commits on your project whenever you reach a helpful checkpoint; say after you finish each part of the assignment. You'll be pushing all of your commits up to GitHub, so this will allow both you and the GSI to keep track of how your work evolved and go back to previous versions if you worked your way into a bind.

After the deadline passes, the GSIs will start working their way though the repos, leaving comments in the "Issues" tab. They'll also mark you as having submitted the Project in the corresponding assignment on bCourses.
   

## :octocat: Background on Git and GitHub

Git is a **distributed Version Control System (VCS)**, which means it is a useful tool for easily tracking changes to your code, collaborating, and sharing. With Git you can track the changes you make to your project so you always have a record of what you’ve worked on and can easily revert back to an older version if need be. It also makes working with others easier—groups of people can work together on the same project and merge their changes into one final source!

GitHub is a way to use the same power of Git all online with an easy-to-use interface. It’s used across the software world and beyond to collaborate and maintain the history of projects.

GitHub is home to some of the most advanced technologies in the world. Whether you're visualizing data or building a new game, there's a whole community and set of tools on GitHub that can get you to the next step. This course starts with the basics of GitHub, but we'll dig into the rest later.

## :octocat: Understanding the GitHub flow 

The GitHub flow is a lightweight workflow that allows you to experiment and collaborate on your projects easily, without the risk of losing your previous work.

### Repositories

A repository is where your project work happens--think of it as your project folder. It contains all of your project’s files and revision history.  You can work within a repository alone or invite others to collaborate with you on those files.

### Cloning 

When a repository is created with GitHub, it’s stored remotely in the ☁️. You can clone a repository to create a local copy on your computer and then use Git to sync the two. This makes it easier to fix issues, add or remove files, and push larger commits. Cloning a repository also pulls down all the repository data that GitHub has at that point in time, including all versions of every file and folder for the project! This can be helpful if you experiment with your project and then realize you liked a previous version more. 
To learn more about cloning, read ["Cloning a Repository"](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository). 

### Committing and pushing
**Committing** and **pushing** are how you can add the changes you made on your local machine to the remote repository in GitHub. That way your instructor and/or teammates can see your latest work when you’re ready to share it. You can make a commit when you have made changes to your project that you want to “checkpoint.” You can also add a helpful **commit message** to remind yourself or your teammates what work you did (e.g. “Added a README with information about our project”).

Once you have a commit or multiple commits that you’re ready to add to your repository, you can use the push command to add those changes to your remote repository. Committing and pushing may feel new at first, but we promise you’ll get used to it 🙂

## 💻 GitHub terms to know 

### Repositories 
We mentioned repositories already, they are where your project work happens, but let’s talk a bit more about the details of them! As you work more on GitHub you will have many repositories which may feel confusing at first. Fortunately, your ["GitHub dashboard"](https://docs.github.com/en/github/setting-up-and-managing-your-github-user-account/about-your-personal-dashboard) helps to easily navigate to your repositories and see useful information about them. Make sure you’re logged in to see it!

Repositories also contain **README**s. You can add a README file to your repository to tell other people why your project is useful, what they can do with your project, and how they can use it. We are using this README to communicate how to learn Git and GitHub with you. 😄 
To learn more about repositories read ["Creating, Cloning, and Archiving Repositories](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/about-repositories) and ["About README's"](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/about-readmes). 

### Your user profile

Your profile page tells people the story of your work through the repositories you're interested in, the contributions you've made, and the conversations you've had. You can also give the world a unique view into who you are with your profile README. You can use your profile to let future employers know all about you! 
To learn more about your user profile and adding and updating your profile README, read ["Managing Your Profile README"](https://docs.github.com/en/github/setting-up-and-managing-your-github-profile/managing-your-profile-readme). 

### Using markdown on GitHub 

You might have noticed already, but you can add some fun styling to your issues, pull requests, and files. ["Markdown"](https://guides.github.com/features/mastering-markdown/) is an easy way to style your issues, pull requests, and files with some simple syntax. This can be helpful to organize your information and make it easier for others to read. You can also drop in gifs and images to help convey your point!
To learn more about using GitHub’s flavor of markdown, read ["Basic Writing and Formatting Syntax"](https://docs.github.com/en/github/writing-on-github/basic-writing-and-formatting-syntax). 

### Engaging with the GitHub community

The GitHub community is vast. There are many types of people who use GitHub in their day to day—students like you, professional developers, hobbyists working on open source projects, and explorers who are just jumping into the world of software development on their own. There are many ways you can interact with the larger GitHub community, but here are three places where you can start. 

#### Starring repositories 

If you find a repository interesting or you want to keep track of it, star it! When you star a repository it’s also used as a signal to surface better recommendations on github.com/explore. If you’d like to get back to your starred repositories you can do so via your user profile. 
To learn  more about starring repositories, read ["Saving Repositories with Stars"](https://docs.github.com/en/github/getting-started-with-github/saving-repositories-with-stars). 

#### Following users 

You can follow people on GitHub to receive notifications about their activity and discover projects in their communities. When you follow a user, their public GitHub activity will show up on your dashboard so you can see all the cool things they are working on. 
To learn more about following users, read ["Following People"](https://docs.github.com/en/github/getting-started-with-github/following-people).

## 📝 Optional next steps 

* Open a pull request and let your teacher know that you’ve finished this course.  
* Create a new markdown file in this repository. Let them know what you learned and what you are still confused about! Experiment with different styles!
* Create your profile README. Let the world know a little bit more about you! What are you interested in learning? What are you working on? What's your favorite hobby? Learn more about creating your profile README in the document, ["Managing Your Profile README"](https://docs.github.com/en/github/setting-up-and-managing-your-github-profile/managing-your-profile-readme).
* Go to your user dashboard and create a new repository. Experiment with the features within that repository to familiarize yourself with them. 
* [Let us know what you liked or didn’t like about the content of this course](https://support.github.com/contact/education). What would you like to see more of? What would be interesting or helpful to your learning journey? 

## 📚  Resources 
* [A short video explaining what GitHub is](https://www.youtube.com/watch?v=w3jLJU7DT5E&feature=youtu.be) 
* [Git and GitHub learning resources](https://docs.github.com/en/github/getting-started-with-github/git-and-github-learning-resources) 
* [Understanding the GitHub flow](https://guides.github.com/introduction/flow/)
* [Interactive Git training materials](https://githubtraining.github.io/training-manual/#/01_getting_ready_for_class)

## My feedback
I think I still need some time to get to fammiliar with the git commands and the way these commands work.
