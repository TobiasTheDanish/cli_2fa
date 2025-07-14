package otp

import (
	"encoding/base32"
	"encoding/hex"
	"fmt"
)

func generateTotp(key string, t uint64, step, digits int) (string, error) {
	counter := t / uint64(step)

	decoded, err := base32.StdEncoding.DecodeString(key)
	if err != nil {
		return "", err
	}
	hexKey := hex.EncodeToString(decoded)

	hotp := generateHOTP(hexKey, counter, digits)

	return fmt.Sprintf("%0*d", digits, hotp), nil
}
