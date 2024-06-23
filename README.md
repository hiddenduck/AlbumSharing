<h1 align="center" id="title">AlbumSharing</h1>

<p id="description">Album Sharing is a distributed system designed to facilitate seamless collaboration and sharing of albums containing multimedia files like photos and videos among users. It allows users to manage their albums upload and download files rate files and engage in real-time discussions related to each album.</p>

<h2>💻 Built with</h2>

## Technologies Used
- **Central Server**: Erlang
- **Data Servers**: Java
- **Clients**: Go
- **Communication**: 
  - TCP/IP sockets (client-server)
  - ZeroMQ (peer-to-peer client communication)
  - reactive-grpc (client-data server communication)

## Features
### Client
- Establishes sessions with the central server for album management.
- Uses TCP/IP for communication with the central server.
- Supports peer-to-peer communication using ZeroMQ for collaborative album editing and real-time chat.
- Interacts with data servers via gRPC for file storage and retrieval.

### Central Server
- Manages user registration and authentication.
- Stores replicas of album metadata.
- Facilitates session management for album editing.

### Data Servers
- Ensure file content persistence using content-addressable storage.
- Utilize consistent hashing for load distribution.
- Support dynamic addition of new servers to the cluster.


### Commands

- **User Registration**
    ```sh
    register <name> <password>
    ```

- **User Login**
    ```sh
    login <name> <password>
    ```

- **Create Album**
    ```sh
    createAlbum <name>
    ```

- **Get Album for Editing**
    ```sh
    getAlbum <name>
    ```

- **Finish Editing Session**
    ```sh
    putAlbum
    ```

- **List Replica**
    ```sh
    listReplica
    ```

- **List Files in Album**
    ```sh
    listFiles
    ```

- **Add User to Album**
    ```sh
    addUser <name>
    ```

- **Remove User from Album**
    ```sh
    removeUser <name>
    ```

- **Add File to Album**
    ```sh
    addFile <name>
    ```

- **Remove File from Album**
    ```sh
    removeFile <name>
    ```

- **Download File**
    ```sh
    downloadFile <name>
    ```
