/*global Elm, firebase */

(function () {
    // Start the Elm App.
    var app = Elm.App.fullscreen();

    // Initialize Firebase
    var firebaseApp = firebase.initializeApp({
        apiKey: 'AIzaSyBG5-dI_sIjAC5KyQn5UEL9CLrhXwuiwgA',
        authDomain: 'voting-e6be5.firebaseapp.com',
        databaseURL: 'https://voting-e6be5.firebaseio.com',
        storageBucket: ''
    });

    // Firebase Auth.
    app.ports.authenticate.subscribe(function () {
        firebase.auth().signInAnonymously()
            .catch(app.ports.authError.send);
    });


    // Looks like this doesn't work at all if it fires before the Elm
    // app has finished initialising. But why hasn't it, by this
    // stage???
    setTimeout(function() {
        firebaseApp.auth()
            .onAuthStateChanged(app.ports.authStateChanged.send);
    }, 1);


    // Voting.
    var votePath = firebase.database().ref('/votes');

    app.ports.voteSend.subscribe(function (msg) {
        var uid = msg[0],
            vote = msg[1],
            path = votePath.child(uid);

        path.set(vote)
            .catch(app.ports.voteSendError.send);
    });

    app.ports.votesListen.subscribe(function () {
        votePath.on('value', function(snapshot) {
            var rawValue, key, pairs = [];

            rawValue = snapshot.val();

            for (key in rawValue) {
                if (rawValue.hasOwnProperty(key)) {
                    pairs.push([key, rawValue[key]]);
                }
            }

            app.ports.votes.send(pairs);
        });
    });

    app.ports.votesSilence.subscribe(function () {
        votePath.off('value');
    });
}());
