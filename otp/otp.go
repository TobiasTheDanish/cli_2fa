package otp

import "time"

type OTP interface {
	Current() string
	FromTime(time.Time) string

	WithStep(s int) OTP
	WithDigits(d int) OTP
}

func New(key string) OTP {
	return &otp{key: key, step: 30, digits: 6}
}

type otp struct {
	key    string
	step   int
	digits int
}

func (o *otp) Current() string {
	return o.FromTime(time.Now())
}

func (o *otp) FromTime(t time.Time) string {
	ut := t.Unix()
	return generateTotp(o.key, uint64(ut), o.step, o.digits)
}

func (o *otp) WithStep(s int) OTP {
	o.step = s
	return o
}
func (o *otp) WithDigits(d int) OTP {
	o.digits = d
	return o
}
