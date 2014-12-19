(ns dns-server.core)

(import '[java.net DatagramSocket
                   DatagramPacket
                   InetSocketAddress])

(defn- ^BigInteger bytes->int
  [^bytes bytes & {:keys [little-endian] :or {little-endian true}}]
    (let [b (if little-endian (reverse bytes) bytes)]
      (->>
        b
        (cons (byte 0))
        (byte-array)
        (biginteger))))

(defn send
  "Send a short textual message over a DatagramSocket to the specified
   host and port. If the string is over 512 bytes long, it will be
   truncated."
   [^DatagramSocket socket msg host port]
   (let [payload (.getBytes msg)
         length (min (alength payload) 512)
         address (InetSocketAddress. host port)
         packet (DatagramPacket. payload length address)]
    (.send socket packet)))

(defn receive
  "Block until a UDP message is received on the given DatagramSocket, and
   return the payload message as a string."
   [^DatagramSocket socket]
   (let [buffer (byte-array 512)
         packet (DatagramPacket. buffer 512)]
    (.receive socket packet)
    (.getData packet)))

(defn process-headers
  "Process the 12-byte header into the appropriate fields"
  [header]
  (let [id-field (bytes->int (byte-array (take 2 header)))
        qr-field (bit-and 1 (first (drop 2 header)))
        opcode-field (bit-and 15 (bit-shift-right (first (drop 2 header)) 1))]
    (hash-map :id-field id-field :qr-field qr-field :opcode-field opcode-field)))

(defn process-questions
  "Process the variable length question field from the DNS request"
  [questions]
  (String. (byte-array questions) 0 (count questions)))

(defn process-data
  "Processes a DNS request into a header and a question"
  [data]
  (let [header (take 12 data)
        questions (drop 12 data)]
    (hash-map :headers (process-headers header) :questions (process-questions questions))))

(defn receive-loop
  "Given a function and DatagramSocket, will (in another thread)
   wait for the socket to receive a message, and whenever it does, will
   call the provided function on the incoming message."
   [socket f]
   (future (while true (f (receive socket)))))

(defn display-data
  "Prints out the data from the packet for debugging purposes"
  [data]
  (let [{headers :headers questions :questions} (process-data data)
        output (map (fn [entry] (str (name (first entry)) ": " (second entry))) headers)]
    (println (str (clojure.string/join "\n" output) "\n" questions "\n"))))

(defn -main
  "Kicks this pig"
  [& args]
  (let [socket (DatagramSocket. 53)]
    (receive-loop socket display-data)))
