These are random development thoughts/log.

### July 31, 2024
The current repo has too much structure, I think it will be fine to do without the distinction between Terminal/better GUI.

Claude provided a good roadmap of development:
1. Work out core functionalities and have a single AI. Forget about servers for now. Implement all the logic.
2. Still one main player and a single AI, but manage them through servers. I.e., server now controls the phase timers, and AI has
to issue commands to the server, etc.
3. flush out server stuff, adding game rooms, authentications, etc.

Note, server and client will be bidirectionally. Server needs to manage phase timers and ping the client when phase changes.
So, the client needs to `forkIO` twice for a sending and receiving thread. The server can have one `fork` per client.