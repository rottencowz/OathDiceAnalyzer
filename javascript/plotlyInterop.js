
// gross - keep track of this in elm-land
var lastRunMessage = undefined;

/**
 * 
 */
function createPlotlyPlotsFromRoll(message, selected = undefined) {
    if (!message) {
        return;
    }

    lastRunMessage = message;
    createRollDistributionHistogram(message, selected);
    createExpectedLossHistogram(message, selected);
}


function reprojectWithSelected(message) {
    createPlotlyPlotsFromRoll(lastRunMessage, message);
}

function createRollDistributionHistogram(message, selected) {
    var xOffense = [];
    var xDefense = [];
    message[1].forEach((elem, i) => {
        xOffense[i] = Math.floor(elem.summarizedOffense[0]);
        xDefense[i] = elem.summarizedDefense[0] * Math.max(1, elem.summarizedDefense[1]);
    });

    var offenseHistogram = {
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
        yaxis: "y"
    };

    if (selected) {
        offenseHistogram = {
            ...offenseHistogram,
            selectedpoints: [selected],
            selected: {
                marker: {
                    color: "red",
                    opacity: 0.8
                }
            }
        }
    }

    var defenseHistogram = {
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
        yaxis: "y"
    };

    if (selected) {
        defenseHistogram = {
            ...defenseHistogram,
            selectedpoints: [selected],
            selected: {
                marker: {
                    color: "green",
                    opacity: 0.8
                }
            }
        }
    }


    var offenseBox = {
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

    var defenseBox = {
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
    var data = [offenseHistogram, defenseHistogram, offenseBox, defenseBox];

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
function createExpectedLossHistogram(message, selected) {
    var wins = [];
    var losses = [];
    var inputs = message[0];
    var boxForIndex = [];
    var selectedIndex = undefined;
    message[1].forEach((elem, i) => {
        var offensiveScore = Math.floor(elem.summarizedOffense[0]);
        var defensiveScore = elem.summarizedDefense[0] * Math.max(1, elem.summarizedDefense[1]) + inputs.defendingTroops;

        var immediateRollLosses = elem.summarizedOffense[1];
        var availableTroopsPostRoll = Math.max(0, inputs.attackingTroops - immediateRollLosses);

        if (offensiveScore > defensiveScore) {
            wins.push(immediateRollLosses);
            boxForIndex.push(true);
            if (i === selected) {
                selectedIndex = wins.length - 1;
            }
        } else if (offensiveScore + availableTroopsPostRoll > defensiveScore) {
            var lossesToWin = (defensiveScore - offensiveScore) + 1;
            wins.push(immediateRollLosses + lossesToWin);
            boxForIndex.push(true);
            if (i === selected) {
                selectedIndex = wins.length - 1;
            }
        } else {
            // TODO: bug
            // handle upper bound of losses here. If someone rolls a dice with 0 troops, can't lose more than 0 troops.
            losses.push(immediateRollLosses + Math.floor(availableTroopsPostRoll / 2));
            boxForIndex.push(false);
            if (i === selected) {
                selectedIndex = losses.length - 1;
            }
        }
    });
    var winnableTrace = {
        x: wins,
        name: 'Victory',
        autobinx: true,
        marker: {
            color: "orange",
            line: {
                color: "red",
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
                color: "purple",
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

    if (selected) {
        if (boxForIndex[selected] === true) {
            winnableTrace = {
                ...winnableTrace,
                selectedpoints: [selectedIndex],
                selected: {
                    marker: {
                        color: "orange",
                        opacity: 0.8
                    }
                }
            };
            guaranteedLossTrace = {
                ...guaranteedLossTrace,
                opacity: 0.2
            }
        } else {
            guaranteedLossTrace = {
                ...guaranteedLossTrace,
                selectedpoints: [selectedIndex],
                selected: {
                    marker: {
                        color: "blue",
                        opacity: 0.8
                    }
                }
            };
            winnableTrace = {
                ...winnableTrace,
                opacity: 0.2
            }
        }
    }

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
