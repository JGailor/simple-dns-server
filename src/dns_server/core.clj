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

(defn process-question
  "Processes a DNS request into a header and a question"
  [data]
  (let [header (take 12 data)
        question (drop 12 data)
        id-field (bytes->int (byte-array (take 2 header)))
        qr-field (bit-and 1 (first (drop 2 header)))
        opcode-field (bit-and 15 (bit-shift-right (first (drop 2 header)) 1))
        header-msg (str "header: " (String. (byte-array header) 0 (count header)))
        question-msg (str "question: " (String. (byte-array question) 0 (count question)))]
    [header-msg id-field qr-field opcode-field question-msg]))

(defn receive-loop
  "Given a function and DatagramSocket, will (in another thread)
   wait for the socket to receive a message, and whenever it does, will
   call the provided function on the incoming message."
   [socket f]
   (future (while true (f (receive socket)))))

(def socket (DatagramSocket. 53))

(receive-loop socket (fn [data] (println (clojure.string/join "\n" (process-question data)))))
