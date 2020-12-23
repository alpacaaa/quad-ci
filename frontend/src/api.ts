
export const endpoint = "http://localhost:9000";
export const refresh = { refreshInterval: 1000 };

// @ts-ignore
export const fetchJson = (...args) => fetch(...args).then((res) => res.json());
// @ts-ignore
export const fetchText = (...args) => fetch(...args).then((res) => res.text());
