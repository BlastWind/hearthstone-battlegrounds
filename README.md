# battlegrounds
Early Work In Progress!

![early-wip-demo](./docs/early-wip-demo.gif)

Overall, the architecture is MVC (Model, View, Controller) and some event-driven programming. See discussions on architecture [here](app/Model.md).

Caveat to those who built locally and are viewing the source in vscode: For reasons unknown (but is related to 
usage of `OverloadedRecordUpdate` and `OverloadedRecordDot`), files dependant on `Model.hs` only typechecks
when `Model.hs` is open.

### Roadmap
1. [x] Basic Game Loop.
2. [ ] Support the follow cards in this order: A God Card (responds to all events), Harmless Bonehead, Picky Eater, Upbeat Frontdrake, Dune Dweller
3. [ ] Support all cards, completing the single player mode experience.
4. [ ] Server, game rooms, authentication.