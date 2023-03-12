// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: '블로그 생성기를 만들면서 배우는 하스켈',
  tagline: '하스켈을 배우기 위한 프로젝트 중심적 온라인 책',
  favicon: 'img/book-logo-transparent.png',

  // Set the production url of your site here
  url: 'https://jbl428.github.io',
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: '/learn-haskell-blog-generator/',

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: 'jbl428', // Usually your GitHub org/user name.
  projectName: 'learn-haskell-blog-generator', // Usually your repo name.

  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',

  // Even if you don't use internalization, you can use this field to set useful
  // metadata like html lang. For example, if your site is Chinese, you may want
  // to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: 'ko',
    locales: ['ko'],
  },

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          editUrl:
            'https://github.com/jbl428/learn-haskell-blog-generator/tree/book',
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      // Replace with your project's social card
      image: 'img/book-logo-transparent.png',
      navbar: {
        title: '블로그 생성기를 만들면서 배우는 하스켈',
        logo: {
          alt: 'haskell',
          src: 'img/book-logo-transparent.png',
        },
        items: [
          {
            type: 'doc',
            docId: 'SUMMARY',
            position: 'left',
            label: 'Book',
          },
          {
            href: 'https://github.com/jbl428/learn-haskell-blog-generator',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ["haskell"],
      },
    }),
};

module.exports = config;
