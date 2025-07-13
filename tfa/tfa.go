package tfa

import (
	"crypto/hmac"
	"crypto/sha1"
	"encoding/base32"
	"encoding/binary"
	"fmt"
	"math"
)

func GenerateTotp(key string, t uint64, step, digits int) (string, error) {
	counter := t / uint64(step)

	decoded, err := base32.StdEncoding.DecodeString(key)
	if err != nil {
		return "", err
	}

	hotp := GenerateHOTP(string(decoded), counter, digits)

	return fmt.Sprintf("%0*d", digits, hotp), nil
}

func GenerateHOTP(key string, counter uint64, digits int) int {
	h := hmac.New(sha1.New, []byte(key))

	msg := make([]byte, 8, 8)
	binary.LittleEndian.PutUint64(msg, counter)
	h.Write(msg)

	hash := h.Sum(nil)

	offset := hash[len(hash)-1] & 0xf

	var bin int
	for i := range 4 {
		bin |= int(hash[offset+byte(i)]) << (8 * (3 * i))
	}

	max := int(math.Pow10(digits))

	return (bin % max)
}
