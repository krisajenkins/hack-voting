/*global Elm, firebase */


(function () {
    // Initialize Firebase
    var config = {
        apiKey: 'AIzaSyBG5-dI_sIjAC5KyQn5UEL9CLrhXwuiwgA',
        authDomain: 'voting-e6be5.firebaseapp.com',
        databaseURL: 'https://voting-e6be5.firebaseio.com',
        storageBucket: ''
    };
    var firebaseApp = firebase.initializeApp(config);
    // Start the Elm App.
    var app = Elm.App.fullscreen();

    var database = firebase.database();
    var votePath = database.ref('/votes');


    app.ports.vote.subscribe(function (msg) {
        var user = msg[0],
            vote = msg[1],
            path = votePath.child(user.uid);

        console.log('Voting', path.toString(), vote);
        path.set(vote)
            .catch(app.ports.voteError.send);
    });


    app.ports.watch.subscribe(function () {
        console.log('Watching', votePath.toString());
        votePath .on('value', function(snapshot) {
            console.log('HEARD value', snapshot.val());
            var rawValue, key, pairs = [];

            rawValue = snapshot.val();

            for(key in rawValue) {
                if (rawValue.hasOwnProperty(key)) {
                    pairs.push([key, rawValue[key]]);
                }
            }
            app.ports.listenToVotes.send(pairs);
        });
    });

    app.ports.authenticate.subscribe(function () {
        firebase.auth()
            .signInAnonymously().catch(app.ports.authError.send);
    });


    // Looks like this doesn't work at all if it fires before the Elm
    // app has finished initialising. But why hasn't it, by this
    // stage???
    setTimeout(function() {
        firebaseApp.auth()
            .onAuthStateChanged(app.ports.authStateChanged.send);
    }, 1);

}());
