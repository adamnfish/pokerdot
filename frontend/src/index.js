import './main.css';
import { Elm } from './Main.elm';
import ReconnectingWebSocket from 'reconnecting-websocket';
import * as serviceWorker from './serviceWorker';


const app = Elm.Main.init({
  node: document.getElementById('root')
});


// Server communication

const socket = new ReconnectingWebSocket(apiUri(location.hostname));

socket.addEventListener('open', function (event) {
  console.log('Websocket connection opened');
  app.ports.socketConnect.send(null);
});

socket.addEventListener('close', function (event) {
  console.log('Websocket connection closed');
  app.ports.socketDisconnect.send(null);
});

socket.addEventListener('error', function (event) {
  console.log('Websocket connection error', event.data);
  app.ports.socketDisconnect.send(null);
});

socket.addEventListener('message', function (event) {
  const eventJson = JSON.parse(event.data);
  console.log('<< Message from server ', eventJson);
  app.ports.receiveMessage.send(eventJson);
});

app.ports.sendMessage.subscribe(function (messageData) {
  console.log('>> Sending message ', messageData);
  socket.send(JSON.stringify(messageData));
});

function apiUri(hostname) {
    if (/(localhost|[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3})/.test(hostname)) {
        return "ws://" + hostname + ":7000/api";
    } else {
        const match = hostname.match(/(\w+?)\.(.*)/);
        return "wss://" + match[1] + "-api." + match[2] + "/"
    }
}


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
