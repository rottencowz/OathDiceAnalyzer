/**
 * 
 */
function createPlotlyPlotsFromRoll(message) {
    createRollDistributionHistogram(message);
    createExpectedLossHistogram(message);
}

function createRollDistributionHistogram(message) {
    var xOffense = [];
    var xDefense = [];
    message[1].forEach((elem, i) => {
        xOffense[i] = Math.floor(elem.summarizedOffense[0]);
        xDefense[i] = elem.summarizedDefense[0] * Math.max(1, elem.summarizedDefense[1]);
    });

    var trace1 = {
        x: xOffense,
        name: 'Offense',
        autobinx: true,
        marker: {
            color: "rgba(255, 200, 100, 0.4)",
            line: {
                color: "rgba(255, 100, 100, 1)",
                width: 2
            }
        },
        opacity: 1,
        type: "histogram",
        histnorm: 'percent',
        xbins: { start: 0 },
        xaxis: "x",
        yaxis: "y",
        // selectedpoints: [],
        // selected: {
        //     marker: {
        //         color: "red",
        //         opacity: 0.3
        //     }
        // }
    };

    var trace2 = {
        x: xDefense,
        autobinx: true,
        marker: {
            color: "rgba(100, 200, 102, 0.3)",
            line: {
                color: "rgba(100, 200, 102, 0.8)",
                width: 2
            }
        },
        name: "Defense",
        opacity: 0.8,
        type: "histogram",
        histnorm: 'percent',
        xbins: { start: 0 },
        xaxis: "x",
        yaxis: "y",
        // selectedpoints: [],
        // selected: {
        //     marker: {
        //         color: "green",
        //         opacity: 0.3
        //     }
        // }
    };


    var trace3 = {
        name: "O",
        x: xOffense,
        marker: {
            color: "rgba(255, 100, 102, 0.7)",
            line: {
                color: "rgba(255, 100, 102, 1)",
                width: 1
            }
        },
        opacity: 0.5,
        type: "box",
        boxmean: 'sd',
        xaxis: 'x',
        yaxis: 'y2',
    };

    var trace4 = {
        name: "D",
        x: xDefense,
        marker: {
            color: "rgba(100, 200, 102, 0.7)",
            line: {
                color: "rgba(100, 200, 102, 1)",
                width: 1
            }
        },
        opacity: 0.5,
        type: "box",
        boxmean: 'sd',
        xaxis: 'x',
        yaxis: 'y2',
    };
    var data = [trace1, trace2, trace3, trace4];

    var layout = {
        bargap: 0.05,
        bargroupgap: 0.2,
        barmode: "overlay",
        title: "Offense vs Defense",
        xaxis: { title: "Roll Values" },
        yaxis: { title: "Probability (%)" },
        grid: { rows: 2, columns: 1 },
    };

    Plotly.newPlot('plotly-distributions', data, layout, { staticPlot: true });
}

/**
 * Determine whether a fight is winnable.
 * OffenseRoll + MaxNumber of sacrifices > Defense Roll + troops
 * If guaranteed loss, expected loss of troops is half remaining force
 * @param {*} message 
 */
function createExpectedLossHistogram(message) {
    var wins = [];
    var losses = [];
    var inputs = message[0];
    message[1].forEach((elem, i) => {
        var offensiveScore = Math.floor(elem.summarizedOffense[0]);
        var defensiveScore = elem.summarizedDefense[0] * Math.max(1, elem.summarizedDefense[1]) + inputs.defendingTroops;

        var immediateRollLosses = elem.summarizedOffense[1];
        var availableTroopsPostRoll = Math.max(0, inputs.attackingTroops - immediateRollLosses);

        if (offensiveScore > defensiveScore) {
            wins.push(immediateRollLosses);
        } else if (offensiveScore + availableTroopsPostRoll > defensiveScore) {
            var lossesToWin = (defensiveScore - offensiveScore) + 1;
            wins.push(immediateRollLosses + lossesToWin);
        } else {
            losses.push(immediateRollLosses + Math.floor(availableTroopsPostRoll / 2));
        }
    });

    var winnableTrace = {
        x: wins,
        name: 'Victory',
        autobinx: true,
        marker: {
            color: "orange",
            line: {
                color: "rgba(255, 100, 102, 1)",
                width: 1
            }
        },
        opacity: 0.5,
        type: "histogram",
        xbins: { start: 0 },
        xaxis: "x",
        yaxis: "y"
    };

    var guaranteedLossTrace = {
        x: losses,
        autobinx: true,
        marker: {
            color: "blue",
            line: {
                color: "rgba(100, 200, 102, 1)",
                width: 1
            }
        },
        name: "Loss",
        opacity: 0.75,
        type: "histogram",
        xbins: { start: 0 },
        xaxis: "x",
        yaxis: "y"
    };
    var layout2 = {
        bargap: 0.05,
        bargroupgap: 0.2,
        barmode: "stack",
        title: "Expected Offensive Losses",
        xaxis: { title: "Losses" },
        yaxis: { title: "Counts" },
    };
    Plotly.newPlot('plotly-expectedLosses', [winnableTrace, guaranteedLossTrace], layout2, { staticPlot: true });
}
