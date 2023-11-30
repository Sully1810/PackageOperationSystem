%% @doc Hello world handler.
-module(default_page_h).

-export([init/2]).

init(Req0, Opts) ->
        Req = cowboy_req:reply(200, #{
                <<"content-type">> => <<"text/plain">>
        }, "<!DOCTYPE html>
        <html>
        <head>
            <title>Package Tracking Simulator</title>
        </head>
        <body>
            <h1>Package Tracking Simulator</h1>
        
            <label for=\"packageCount\">Number of Packages:</label>
            <input type=\"number\" id=\"packageCount\" value=\"1\"><br>
        
            <label for=\"requestsPerType\">Requests per Type for Each Package:</label>
            <input type=\"number\" id=\"requestsPerType\" value=\"1\"><br>
        
            <button id=\"startButton\">Start Simulation</button>
            <div id=\"log\"></div>
            <script>document.getElementById('startButton').addEventListener('click', startSimulation);
        
                function startSimulation() {
                    const packageCount = parseInt(document.getElementById('packageCount').value) || 1;
                    const requestsPerType = parseInt(document.getElementById('requestsPerType').value) || 1;
                    const logElement = document.getElementById('log');
                    logElement.innerHTML = ''; // Clear log
                
                    for (let i = 0; i < packageCount; i++) {
                        const packageUuid = generateUUID();
                
                        for (let j = 0; j < requestsPerType; j++) {
                            const packageUpdateData = buildPackageUpdate(packageUuid);
                            sendRequest('/pkg_upd', packageUpdateData, (response) => {
                                logElement.innerHTML += `<p>Package Update for Package ${i+1}, Request ${j+1}: ${response}</p>`;
                            });
                
                            const locationUpdateData = buildLocationUpdate(packageUuid);
                            sendRequest('/rpt_loc', locationUpdateData, (response) => {
                                logElement.innerHTML += `<p>Location Update for Package ${i+1}, Request ${j+1}: ${response}</p>`;
                            });
                
                            const packageLocationRequestData = buildPackageLocationRequest(packageUuid);
                            sendRequest('/pkg_loc', packageLocationRequestData, (response) => {
                                logElement.innerHTML += `<p>Package Location Request for Package ${i+1}, Request ${j+1}: ${response}</p>`;
                            });
                
                            const deliveredRequestData = buildDeliveredRequest(packageUuid);
                            sendRequest('/delivered', deliveredRequestData, (response) => {
                                logElement.innerHTML += `<p>Delivered Request for Package ${i+1}, Request ${j+1}: ${response}</p>`;
                            });
                        }
                    }
                }
                
                
                
                function sendRequest(endpoint, data, callback) {
                    // Use fetch API to send requests to your server
                    fetch(endpoint, {
                        method: 'POST',
                        headers: {
                            'Content-Type': 'application/json',
                        },
                        body: JSON.stringify(data),
                    })
                    .then(response => response.text())
                    .then(callback)
                    .catch(error => console.error('Error:', error));
                }
                
                function generateUUID() {
            return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
                var r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
                return v.toString(16);
            });
        }
        
                function buildPackageUpdate(packageUuid) {
                    return {
                        pkg_uuid: packageUuid,
                        loc_uuid: generateUUID(), // Simulating generate_UUID(\"someLocation.blah\", not_real_location)
                        time: Math.round(Math.random() * 1_000_000)
                    };
                }
                
                function buildLocationUpdate(packageUuid) {
                    return {
                        loc_uuid: generateUUID(), // Simulating generate_UUID(\"someLocation.blah\", not_real_location)
                        lat: Math.round(Math.random() * 360),
                        long: Math.round(Math.random() * 360),
                        time: Math.round(Math.random() * 1_000_000)
                    };
                }
                
                function buildPackageLocationRequest(packageUuid) {
                    return {
                        uuid: packageUuid
                    };
                }
                
                function buildDeliveredRequest(packageUuid) {
                    return {
                        uuid: packageUuid,
                        time: new Date().getTime(), // Simulating time_in_millis()
                        lat: Math.round(Math.random() * 360),
                        long: Math.round(Math.random() * 360)
                    };
                }
                </script>
        </body>
        </html>
        ", Req0),
        {ok, Req, Opts}.