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

(defn hash-entry-to-string
  "Converts a hash entry to printable string"
  [entry width]
  (str (format (str "%-" width "s") (str (name (first entry)) ": ")) (second entry)))

(defn hash-to-string
  "Converts a hash to a printable string"
  [hashy-thing width]
  (clojure.string/join "\n" (map #(hash-entry-to-string % width) hashy-thing)))

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
  (let [id-field     (bytes->int (byte-array (take 2 header)))
        byte3        (first (drop 2 header))
        byte4        (first (drop 3 header))
        qr-field     (bit-and 2r00000001 byte3)
        opcode-field (bit-and 2r00011110 byte3)
        aa-field     (bit-and 2r00100000 byte3)
        tc-field     (bit-and 2r01000000 byte3)
        rd-field     (bit-and 2r10000000 byte3)
        ra-field     (bit-and 2r00000001 byte4)
        z-field      (bit-and 2r00001110 byte4)
        rcode-field  (bit-and 2r11110000 byte4)
        qd-count     (bytes->int (byte-array (take 2 (drop 4 header))))
        an-count     (bytes->int (byte-array (take 2 (drop 6 header))))
        ns-count     (bytes->int (byte-array (take 2 (drop 8 header))))
        ar-count     (bytes->int (byte-array (take 2 (drop 10 header))))]
    (hash-map :id-field id-field
              :qr-field qr-field
              :opcode-field opcode-field
              :aa-field aa-field
              :tc-field tc-field
              :rd-field rd-field
              :ra-field ra-field
              :z-field z-field
              :rcode-field rcode-field
              :qd-count qd-count
              :an-count an-count
              :ns-count ns-count
              :ar-count ar-count)))

(defn domain-length
  "Returns the length of bytes of the domain"
  ([bytes] (domain-length bytes 0))
  ([bytes length] (if (= 0 (first bytes) (second bytes)) length (domain-length (rest bytes) (inc length)))))

(defn process-domain
  "Converts the '.' offset + ascii-chars to fully-qualified domain name"
  [bytes]
  (if (empty? bytes)
    []
    (let [offset (first bytes)
          remainder (rest bytes)]
      (concat (take offset remainder) (list (byte \.)) (process-domain (drop offset remainder))))))

(defn process-questions
  "Process the variable length question field from the DNS request"
  [questions]
  (let [domain-bytes (domain-length questions)
        full-domain (process-domain (take domain-bytes questions))]
    (hash-map :domain (String. (byte-array full-domain)))))

(defn process-data
  "Processes a DNS request into a header and a question"
  [data]
  (let [headers (process-headers (take 12 data))
        questions (process-questions (drop 12 data))]
    (hash-map :headers headers :questions questions)))

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
        header-output (hash-to-string headers 15)
        questions-output (hash-to-string questions 15)]
    (println (str header-output "\n" questions-output "\n"))))

(defn -main
  "Kicks this pig"
  [& args]
  (let [socket (DatagramSocket. 53)]
    (receive-loop socket display-data)))
