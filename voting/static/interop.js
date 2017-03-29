/*global Elm, firebase, firebaseConfig */

(function () {
    var startup = function () {
        // Start the Elm App.
        var app = Elm.App.fullscreen();

        // Initialize Firebase
        var firebaseApp = firebase.initializeApp(firebaseConfig);

        // Firebase Auth.
        app.ports.authenticate.subscribe(function () {
            firebase.auth().signInAnonymously()
                .catch(app.ports.authError.send);
        });

        // Event.
        var eventsPath = firebase.database().ref('/events');

        app.ports.eventListen.subscribe(function (eventId) {
            var eventPath = eventsPath.child(eventId);

            console.log('LISTENING', eventPath.toString());
            eventPath.on(
                'value',
                function(snapshot) {
                    var rawValue = snapshot.val();
                    console.log('HEARD', rawValue);

                    app.ports.event.send([eventId, JSON.stringify(rawValue)]);
                },
                app.ports.eventError.send
            );
        });

        app.ports.eventSilence.subscribe(function (eventId) {
            var eventPath = eventsPath.child(eventId);

            console.log('SILENCING', eventPath.toString());
            eventPath.off('value');
        });

        // Voting.
        app.ports.voteSend.subscribe(function (msg) {
            var eventId = msg[0],
                uid = msg[1],
                vote = msg[2];
            var eventPath = eventsPath.child(eventId);
            var votePath = eventPath.child('votes');
            var userPath = votePath.child(uid);

            userPath.set(vote)
                .catch(app.ports.voteSendError.send);
        });

        // Looks like this doesn't work at all if it fires before the Elm
        // app has finished initialising. But why hasn't it, by this
        // stage???
        setTimeout(function() {
            firebaseApp.auth()
                .onAuthStateChanged(app.ports.authStateChanged.send);
        }, 1);
    };

    addEventListener('load', startup, false);
}());
