package otp

import (
	"crypto/hmac"
	"crypto/sha1"
	"encoding/binary"
	"encoding/hex"
	"fmt"
	"math"
)

func generateHOTP(key string, counter uint64, digits int) int {
	k := make([]byte, len([]byte(key)))
	hex.Decode(k, []byte(key))
	h := hmac.New(sha1.New, []byte(k))

	msg := make([]byte, 8)
	binary.BigEndian.PutUint64(msg, counter)
	h.Write(msg)

	hash := h.Sum(nil)

	fmt.Printf("hash: %s\n", hex.EncodeToString(hash))

	offset := hash[len(hash)-1] & 0xf

	var bin int
	for i := range 4 {
		mask := byte(0xff)
		if i == 0 {
			mask = 0x7f
		}
		bin |= int(hash[offset+byte(i)]&mask) << (8 * (3 - i))
	}

	max := int(math.Pow10(digits))

	return (bin % max)
}
