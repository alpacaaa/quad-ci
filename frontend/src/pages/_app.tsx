import "@/style.css";
import { AppProps } from "next/app";

export default function App({ Component, pageProps }: AppProps) {
  return (
    <div className="max-w-5xl mx-auto py-6">
      <header className="mb-6 flex justify-between items-center">
        <a href="/">
          <h1 className="text-sm uppercase font-bold py-1 border-b-4 text-gray-800 border-gray-800">
            Quad CI
          </h1>
        </a>
      </header>
      <Component {...pageProps} />
    </div>
  );
}
