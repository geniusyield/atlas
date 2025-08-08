# Contributing to Atlas

Welcome! We're happy to have you join our community and contribute to one of the most powerful application backends in the Cardano ecosystem.

To learn more about Atlas, check out the [documentation](https://atlas-app.io/). 

## Opening Issues

Use an [existing template](https://github.com/geniusyield/atlas/issues/new/choose) to:
- Report a bug
- Update the docs
- Request a new feature

Accompany your issue with one or more of the following labels:
- `bug`
- `dependencies`
- `documentation`
- `duplicate`
- `github actions`
- `good first issue`
- `haskell`
- `help wanted`
- `vulnerability`
- `wont fix`

## Opening Pull Requests
After opening a pull request, please take the following steps:
- Ensure your PR has one or more corresponding issues (see above)
- [Link your PR](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue) to those issues
- Check the [build status](https://github.com/geniusyield/atlas/actions/new) of your PR
- Wait for a review and/or approval from one of the code owners
- Merge and celebrate!

The team will do their best to help you get your PRs across the finish line!

## Discussions
Feel free to participate in [discussions](https://github.com/geniusyield/atlas/discussions) with the team and community! 

## Project structure

We aim to keep all high-level on-chain code in [`plutus`](./plutus/) folder which is independent of main Atlas code (Atlas only refers to final low level compiled UPLC). This is helpful in avoiding dependency upon Plinth plugin which is locked to a very specific GHC version.